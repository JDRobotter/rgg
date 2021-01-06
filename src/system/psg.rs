
use crate::audio::AudioSynth;
use crate::audio::AudioSynthAction as ASA;
use crate::audio::AudioSynthCommand as ASC;

#[derive(Clone,Copy)]
struct ToneGeneratorRegister {
    divider: u16,
}


/// Sega Game Gear PSG chip
/// https://www.smspower.org/uploads/Development/SN76489-20030421.txt
pub struct PSG {
    audio_synth: AudioSynth,
    latched_byte: u8,

    latched_tone_generator_registers: [ToneGeneratorRegister; 3],

    // debug breakpoint latch
    will_break: bool,
}

impl PSG {

    pub fn new() -> PSG {

        PSG {
            audio_synth: AudioSynth::new(),
            latched_byte: 0,

            latched_tone_generator_registers: [ToneGeneratorRegister {divider:0}; 3],

            will_break: false,
        }
    }

    pub fn will_break(&mut self) -> bool {
        if self.will_break {
            self.will_break = false;
            true
        }
        else {
            false
        }
    }

    fn frequency_from_divider(div:u16) -> f64 {
        // https://www.smspower.org/Development/SN76489?from=Development.PSG#SN76489RegisterWrites
        // when the half-wavelength (tone value) is set to 1, they output a DC offset value
        // corresponding to the volume level
        if div <= 1 {
            return 0.0
        }
        const N:u32 = 3579454;
        let f = N / (32 * div as u32);
        f as f64
    }

    fn amplitude_from_attenuation(a:u8) -> f64 {

        if a == 0x0f {
            // $F is a special case where tone generator is OFF
            0.0
        }
        else {
            // attenuation in db
            let att_db = 2.0 * a as f64;
            32767.0 * (10.0f64).powf(-0.1*att_db)
        }
    }

    pub fn synchronize_timing(&mut self, ncycle:i64) {
        self.audio_synth.push(ASC::new(ncycle, ASA::SyncTiming))
    }

    pub fn write(&mut self, byte: u8, ncycle:i64) {

        // first byte (latched)
        // 1  R1 R0 T  F3 F2 F1 F0
        //
        // R1R0 : register address
        // T : 1: volume / 0: tone
        //
        // second byte
        // 0  x  F9 F8 F7 F6 F5 F4
        //
        // R : register address
        // F : data

        if byte & 0x80 != 0 {
            // LATCH/DATA byte
            self.latched_byte = byte;
        }

        // register address
        let ra = (self.latched_byte >> 5) & 0x03;

        if self.latched_byte & 0x10 != 0 {
            // volume/tone selector

            let att = byte & 0x0f;
            if ra == 3 {
                // noise register volume
                self.audio_synth.push(ASC::new(ncycle,
                    ASA::SetNoiseAmplitude(PSG::amplitude_from_attenuation(att))
                ));
            }
            else {
                // tone register volume
                self.audio_synth.push(ASC::new(ncycle,
                    ASA::SetToneAmplitude(ra.into(), PSG::amplitude_from_attenuation(att))
                ));
            }

        }
        else if ra == 3 {
            // noise register
            
            // writing to noise register, reset internal state
            self.audio_synth.push(ASC::new(ncycle, ASA::ResetNoiseRegister));

            match byte & 0x03 {
                0x00 => {
                    self.audio_synth.push(ASC::new(ncycle,
                        ASA::SetNoiseFrequency(false, PSG::frequency_from_divider(16))
                    ))
                },
                0x01 => {
                    self.audio_synth.push(ASC::new(ncycle,
                        ASA::SetNoiseFrequency(false, PSG::frequency_from_divider(32))
                    ))
                },
                0x02 => {
                    self.audio_synth.push(ASC::new(ncycle,
                        ASA::SetNoiseFrequency(false, PSG::frequency_from_divider(64))
                    ))
                },
                // link to tone generator 3
                0x03 => {
                    self.audio_synth.push(ASC::new(ncycle,
                        ASA::SetNoiseFrequency(true, 0.0)
                    ))
                },
                _ => {},
            }

            self.audio_synth.push(ASC::new(ncycle,
                ASA::SetNoiseFeedback(byte & 0x04 != 0)
            ));

        }
        else {
            // tone registers
            let mut div = self.latched_tone_generator_registers[ra as usize].divider;

            let ubyte = byte as u16;
            if byte & 0x80 != 0 {
                div = (div & 0xff00) | ((ubyte) & 0x000f);
            }
            else {
                div = (div & 0x00ff) | ((ubyte << 4) & 0x0ff0);
            }

            self.latched_tone_generator_registers[ra as usize].divider = div;

            self.audio_synth.push(ASC::new(ncycle,
                ASA::SetToneActive(ra.into(), div != 0)
            ));
            self.audio_synth.push(ASC::new(ncycle,
                ASA::SetToneFrequency(ra.into(), PSG::frequency_from_divider(div))
            ));

        }

    }

}
