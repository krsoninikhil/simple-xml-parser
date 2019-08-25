use simple_xml_parser::{pair,match_literal,identifier};

fn main() {
    let sample = "<parent attr=\"value\"><child1></child1></parent>";

    let node_parser = pair(match_literal("<"), identifier);
    let result = node_parser(sample).unwrap().1;

    println!("node name: {:?}", result.1);
}
