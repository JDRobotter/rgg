#[derive(Clone, Debug)]
struct Leaf<T> {
    instruction: Option<T>,
    children: Option<Vec<Leaf<T>>>,
}

impl<T> Leaf<T> 
    where T: Clone + Copy
{
    fn empty() -> Self {
        Self {
            instruction: None,
            children: None,
        }
    }

    fn with_instruction(instruction:T) -> Self{
        Self {
            instruction: Some(instruction),
            children: None,
        }
    }

    fn get_children(&self, index:usize) -> Option<&Leaf<T>>{
        match self.children.as_ref() {
            Some(children) => Some(&children[index]),
            None => None,
        }
    }

    fn get_children_mut(&mut self, index:usize) -> Option<&mut Leaf<T>>{
        if self.children.is_none() {
            let mut v = vec!();
            v.resize(256, Leaf::empty());
            self.children = Some(v);
        }
        match self.children.as_mut() {
            Some(children) => Some(&mut children[index]),
            None => None,
        }
    }

    fn add_children(&mut self, index:usize, leaf: Leaf<T>) {
        // initialize an empty children tree
        if self.children.is_none() {
            let mut v = vec!();
            v.resize(256, Leaf::empty());
            self.children = Some(v);
        }

        let children = self.children.as_mut().unwrap();
        children[index] = leaf;
    }
}

pub struct Decoder<T> {
    tree: Leaf<T>,
    in_buffer: Vec<u8>,
}

#[derive(Debug,Clone)]
pub enum DecoderState<T> {
    Init,
    Incomplete,
    Complete(T),
    Unknown(Vec<u8>),
}

impl<T> Decoder<T> 
    where T: Clone + Copy {

    pub fn new() -> Self {

        Decoder {
            tree: Leaf::empty(),
            in_buffer: vec!(),
        }
    }

    fn register_to_leaf(tree:&mut Leaf<T>, bytes:&[u8], ins:T) {

        if let Some((byte,rembytes)) = bytes.split_first() {

            let index = *byte as usize;

            if rembytes.is_empty() {
                // end of instruction, push instruction to leaf
                tree.add_children(index, Leaf::with_instruction(ins));
            }
            else {
                // instruction continue
                let branch = tree.get_children_mut(index).unwrap();
                Decoder::register_to_leaf(branch, rembytes, ins);
            }
    
        }
    }

    pub fn register<const N: usize>(&mut self, bytes: [u8;N], ins:T) {
        Decoder::register_to_leaf(&mut self.tree, &bytes[..], ins);
    }

    fn decode_buffer(tree: &Leaf<T>, wbytes: &[u8], all_bytes: &[u8]) -> DecoderState<T> {
        if let Some((byte,rembytes)) = wbytes.split_first() {
            let index = *byte as usize;
            match tree.get_children(index) {
                Some(branch) => { 
                    if rembytes.is_empty() {
                        // end of instruction
                        match branch.instruction {
                            Some(ins) => { return DecoderState::Complete(ins) }
                            None => { return DecoderState::Incomplete }
                        }
                    }
                    else {
                        // intruction continue
                        return Decoder::decode_buffer(branch, rembytes, all_bytes);
                    }
                },
                None => {
                    return DecoderState::Unknown(all_bytes.try_into().unwrap());
                }
            }

        }
        else {
            return DecoderState::Incomplete;
        }
    }
    
    pub fn decode(&mut self, byte:u8) -> DecoderState<T> {
        self.in_buffer.push(byte);

        let state = Decoder::decode_buffer(&self.tree, &self.in_buffer, &self.in_buffer);

        match state {
            DecoderState::Complete(_) => { self.in_buffer.clear() },
            DecoderState::Unknown(_) => { self.in_buffer.clear() },
            _ => {},
        };

        state
    }

}


