use std::vec::Vec;
use std::cmp;

pub struct ScalarStatistics {
    mean: f64,
    max: f64,
    min: f64,

    size: usize,

    buffer: Vec<f64>,
    sum: f64,
}

impl ScalarStatistics {
    pub fn new(n:usize) -> ScalarStatistics {
        ScalarStatistics {
            mean:0.0,
            max:0.0,
            min:0.0,

            size: n,

            buffer: Vec::with_capacity(n),
            sum: 0.0,
        }
    }

    pub fn update(&mut self, v:f64) {
            
        // pop from buffer if next push will oversize it
        if self.buffer.len() >= self.size {
            let pv = self.buffer.pop().unwrap();
            self.sum -= pv;
        }

        // push buffer and update rolling sum
        self.buffer.push(v);
        self.sum += v;

        // update stats
        let sum:f64 = self.buffer.iter().sum();
        let n = self.buffer.len() as f64;
        self.mean = sum / n;

        self.max = self.buffer.iter().fold(f64::MIN, |acc,x| if acc > *x { acc } else { *x });
        self.min = self.buffer.iter().fold(f64::MAX, |acc,x| if acc < *x { acc } else { *x });
    }

    pub fn max(&self) -> f64 {
        self.max
    }

    pub fn min(&self) -> f64 {
        self.min
    }

    pub fn mean(&self) -> f64 {
        self.mean
    }
}
