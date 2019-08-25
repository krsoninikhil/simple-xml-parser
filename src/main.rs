use simple_xml_parser::{match_literal,identifier};

fn main() {
    let sample = "<parent attr=\"value\"><child1></child1></parent>";

    let node_parser = match_literal("<");
    let node_name = identifier(node_parser(sample).unwrap().0);

    println!("node name: {}", node_name.unwrap().1);
}
