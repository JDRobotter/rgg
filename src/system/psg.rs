
use crate::audio::AudioSynth;

use std::thread;

/// Sega Game Gear PSG chip
/// https://www.smspower.org/uploads/Development/SN76489-20030421.txt
pub struct PSG {
    audio_synth: AudioSynth,
    latched_byte: u8,
}

impl PSG {

    pub fn new() -> PSG {

        PSG {
            audio_synth: AudioSynth::new(),
            latched_byte: 0,
        }
    }

    fn frequency_from_divider(div:u16) -> f64 {
        if div == 0 {
            return 0.0
        }
        const N:u32 = 3579454;
        let f = N / (32 * div as u32);
        f as f64
    }

    fn amplitude_from_attenuation(a:u16) -> f64 {

        // attenuation in db
        let att_db = 0.2 * a as f64;

        32767.0 * (10.0f64).powf(-att_db)
    }

    pub fn execute_command(&mut self, addr: u8, data: u16) {
        match addr {
            0x00 => {
                // tone 1 frequency
                self.audio_synth.set_tone_active(0, data != 0);
                self.audio_synth.set_tone_frequency(0, PSG::frequency_from_divider(data));
            },
            
            0x01 => {
                // tone 1 attenuation
                self.audio_synth.set_tone_frequency(0, PSG::amplitude_from_attenuation(data));
            },

            0x02 => {
                // tone 2 frequency
                self.audio_synth.set_tone_active(1, data != 0);
                self.audio_synth.set_tone_frequency(1, PSG::frequency_from_divider(data));
            },

            0x03 => {
                // tone 2 attenuation
                self.audio_synth.set_tone_frequency(1, PSG::amplitude_from_attenuation(data));
            },

            0x04 => {
                // tone 3 frequency
                self.audio_synth.set_tone_active(2, data != 0);
                self.audio_synth.set_tone_frequency(2, PSG::frequency_from_divider(data));
            },

            0x05 => {
                // tone 3 attenuation
                self.audio_synth.set_tone_frequency(2, PSG::amplitude_from_attenuation(data));
            },

            0x07 => {
                // ??
            },

            _ => { panic!("unknown register address: {:02x}", addr); },
        }
    }

    pub fn write(&mut self, byte: u8) {

        // first byte (latched)
        // 1  R2 R1 R0 F3 F2 F1 F0
        //
        // second byte
        // 0  x  F9 F8 F7 F6 F5 F4a
        //
        // R : register address
        // F : data

        if byte & 0x80 != 0 {
            // this byte is latched
            self.latched_byte = byte;

        }
        else {

            let lbyte = self.latched_byte;
            let addr = (lbyte >> 4) & 0x07;
            let data = (lbyte       & 0x0f) as u16
                     | (((byte & 0x3f) as u16) << 4);
            
            self.execute_command(addr, data);
        }
    }


}
