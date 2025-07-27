import os
import json
import argparse
from docutils import core, nodes
import panflute as pf

def rst_to_markdown(rst_text):
    """Convert reStructuredText to Markdown using Panflute (Pandoc)."""
    return pf.convert_text(rst_text, input_format="rst", output_format="markdown")

def extract_vcl_variables(rst_text, source_file=None):
    document = core.publish_doctree(rst_text)
    results = []

    children = list(document.traverse())
    i = 0
    while i < len(children):
        node = children[i]

        if isinstance(node, nodes.paragraph) and 'names' in node:
            variable = (node['names'] or [None])[0]
            if not variable:
                i += 1
                continue
            docs_rst = ""
            fields = {
                "type": "",
                "readable_from": "",
                "writable_from": ""
            }

            i += 1

            # Check next sibling: should be a block_quote
            if i + 1 < len(children) and isinstance(children[i + 1], nodes.block_quote):
                print("ja?")
                block = children[i + 1]
                for para in block.traverse(nodes.paragraph):
                    text = para.astext()
                    if text.lower().startswith("type:"):
                        fields["type"] = text[5:].strip()
                    elif text.lower().startswith("readable from:"):
                        fields["readable_from"] = text[14:].strip()
                    elif text.lower().startswith("writable from:"):
                        fields["writable_from"] = text[14:].strip()
                    else:
                        docs_rst += text.strip() + "\n\n"
                i += 1  # skip block_quote node
            else:
                print("nei.")
                print("sjekk her:", children[i + 1])

            entry = {
                "variable": variable,
                "type": fields["type"],
                "readable_from": [s.strip() for s in fields["readable_from"].split(",") if s.strip()],
                "writable_from": [s.strip() for s in fields["writable_from"].split(",") if s.strip()],
                "docs_md": rst_to_markdown(docs_rst).strip(),
            }
            if source_file:
                entry["source_file"] = source_file
            results.append(entry)

        i += 1

    return results

def main():
    parser = argparse.ArgumentParser(description="Extract VCL variable docs from reStructuredText and output as JSON.")
    parser.add_argument("input", help="Path to input .rst file")
    parser.add_argument("output", help="Path to output .json file")
    args = parser.parse_args()

    with open(args.input, "r", encoding="utf-8") as f:
        rst_text = f.read()

    variables = extract_vcl_variables(rst_text, source_file=os.path.basename(args.input))

    with open(args.output, "w", encoding="utf-8") as f:
        json.dump(variables, f, indent=2)

    print(f"Extracted {len(variables)} variables to {args.output}")

if __name__ == "__main__":
    main()
