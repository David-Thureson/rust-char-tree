mod char_node {

	pub trait CharNode {

		fn add_word(&mut self, s: &str);
		
		fn freeze(&mut self);
		
		fn unfreeze(&mut self);
		
	}
	
}
