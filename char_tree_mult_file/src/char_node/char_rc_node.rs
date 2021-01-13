/*
pub struct CharRcNode<'a> {
    c: char,
	depth: usize,
    parent: Option<Rc<RefCell<CharRcNode<'a>>>>,
	children: HashMap<char, Rc<RefCell<CharRcNode<'a>>>>,
	is_word: bool,
	is_frozen: bool,
	node_count: Option<usize>,
	word_count: Option<usize>,
	height: Option<usize>,
	// self_rc: Option<Rc<RefCell<CharRcNode>>>,
}

impl <'a> CharRcNode<'a> {

	pub fn make_root() -> CharRcNode<'a> {
		let c = ' ';
		let depth = 0;
		let parent = None;
		let is_word = false;
		CharRcNode::new(c, parent, depth, is_word)
	}
	
	// fn new(c: char, parent: RefCell<rc::Weak<CharRcNode>>, is_word: bool) -> CharRcNode {
	fn new(c: char, parent: Option<Rc<RefCell<CharRcNode<'a>>>>, depth: usize, is_word: bool) -> CharRcNode<'a> {
		let children = HashMap::new();
		let node = CharRcNode {
			c,
			depth,
			parent,
			children,
			is_word,
			is_frozen: false,
			node_count: None,
			word_count: None,
			height: None,
			// self_rc: None,
		};
		// let self_rc = Some(Rc::new(RefCell::new(node)));
		// self_rc.unwrap().borrow_mut().self_rc = self_rc.clone();
		// node.self_rc = Some(Rc::new(RefCell::new(node)));
		// let self_rc = Some(Rc::new(RefCell::new(node)));
		// self_rc.unwrap().borrow_mut().self_rc = self_rc.clone();
		node
	}
	
	fn add_from_vec_chars(&mut self, v: Vec<char>, v_len: usize, char_index: usize) {
		assert!(!self.is_frozen);
		if char_index < v_len {
			let c = v[char_index];
			let is_word = char_index == v_len - 1;
			let child_node_opt = self.children.get(&c);
			if let Some(child_node_rc) = child_node_opt {
				let mut child_node = child_node_rc.borrow_mut();
				if is_word {
					child_node.is_word = true;
				}
				child_node.add_from_vec_chars(v, v_len, char_index + 1);
			} else {
				let self_as_parent_rc: Rc<RefCell<CharRcNode>>;
				if let Some(self_parent_rc) = &self.parent {
					// The node has a parent, so use the parent to get the Rc for the current node and clone that.
					self_as_parent_rc = self_parent_rc.borrow().children.get(&(self.c)).unwrap().clone();
				} else {
					self_as_parent_rc = Rc::new(RefCell::new(*self));
				}
				let mut new_child_node = CharRcNode::new(c, Some(self_as_parent_rc), self.depth + 1, is_word);
				new_child_node.add_from_vec_chars(v, v_len, char_index + 1);
				self.children.insert(c, Rc::new(RefCell::new(new_child_node)));
			}
		}
	}
	
	pub fn node_count(&self) -> usize {
		if self.is_frozen {
			self.node_count.unwrap()
		} else {
			let mut calc_count = 1;
			for child_node in self.children.values().map(|x| x.borrow()) {
				calc_count += child_node.node_count();
			}
			calc_count
		}
	}

	pub fn height(&self) -> usize {
		if self.is_frozen {
			self.height.unwrap()
		} else {
			let mut max_child_height = 0;
			for child_node in self.children.values().map(|x| x.borrow()) {
				let child_height = child_node.height();
				if child_height > max_child_height {
					max_child_height = child_height;
				}
			}
			max_child_height + 1
		}
	}
	
	pub fn word_count(&self) -> usize {
		if self.is_frozen {
			self.word_count.unwrap()
		} else {
			let mut count = if self.is_word { 1 } else { 0 };
			for child_node in self.children.values().map(|x| x.borrow()) {
				count += child_node.word_count();
			}
			count
		}
	}

	pub fn freeze(&mut self) {
		if !self.is_frozen {
			let mut node_count = 1;
			let mut word_count = if self.is_word { 1 } else { 0 };
			let mut max_child_height = 0;
			for mut child_node in self.children.values().map(|x| x.borrow_mut()) {
				child_node.freeze();
				node_count += child_node.node_count.unwrap();
				word_count += child_node.word_count.unwrap();
				max_child_height = cmp::max(max_child_height, child_node.height.unwrap());
			}
			self.node_count = Some(node_count);
			self.word_count = Some(word_count);
			self.height = Some(max_child_height + 1);
			self.is_frozen = true;
		}
	}

	pub fn unfreeze(&mut self) {
		if self.is_frozen {
			for mut child_node in self.children.values().map(|x| x.borrow_mut()) {
				child_node.unfreeze();
			}
			self.node_count = None;
			self.word_count = None;
			self.height = None;
			self.is_frozen = false;
		}
	}
	
    pub fn describe_one_line(&self) -> String {
		let is_frozen_desc = if self.is_frozen { " (frozen)" } else { "" };
		let is_word_desc = if self.is_word { " (word)" } else { "" };
		let node_count_desc = format!("; nodes = {}", self.node_count());
		let word_count_desc = format!("; words = {}", self.word_count());
		let depth_desc = format!("; depth = {}", self.depth);
		let height_desc = format!("; height = {}", self.height());
        format!("CharRcNode: {:?}{}{}{}{}{}{}", self.c, is_frozen_desc, is_word_desc, node_count_desc, word_count_desc, depth_desc, height_desc)
    }

    pub fn describe_deep(&self, s: &mut String, depth: usize) {
        s.push_str(&format!(
            "{}\n",
            format_indent(depth, &(self.describe_one_line()))
        ));
        if depth < DEBUG_TREE_MAX_DEPTH {
            for child_node in self.children.values().map(|x| x.borrow()).take(DEBUG_TREE_MAX_CHILDREN) {
				child_node.describe_deep(s, depth + 1);
            }
        }
    }

}

impl CharNode for CharRcNode<'_> {

	fn add_word(&mut self, s: &str) {
		assert!(!self.is_frozen);
		let v: Vec<char> = s.to_lowercase().chars().collect();
		let v_len = v.len();
		self.add_from_vec_chars(v, v_len, 0);
	}
	
}

impl Debug for CharRcNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let mut s = String::new();
            self.describe_deep(&mut s, 0);
            write!(f, "{}", s)
        } else {
            let s = self.describe_one_line();
            write!(f, "{}", s)
        }
    }
}
*/
