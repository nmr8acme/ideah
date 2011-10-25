package ideah.tree;

import ideah.util.LineColRange;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public final class TreeParser {

    private final BufferedReader rdr;

    public TreeParser(Reader rdr) {
        this.rdr = new BufferedReader(rdr);
    }

    private FakeNode readNode() throws IOException {
        String str = rdr.readLine();
        if (str == null)
            return null;
        str = str.trim();
        if ("}".equals(str))
            return null;
        int p1 = str.indexOf(' ');
        String name = str.substring(0, p1).trim();
        String rest = str.substring(p1 + 1).trim();
        int p2 = rest.indexOf(' ');
        String locStr = rest.substring(0, p2).trim();
        LineColRange loc = new LineColRange(locStr);
        List<FakeNode> children = readNodes();
        return new FakeNode(loc, name, children);
    }

    private List<FakeNode> readNodes() throws IOException {
        List<FakeNode> nodes = new ArrayList<FakeNode>();
        while (true) {
            FakeNode node = readNode();
            if (node == null)
                break;
            nodes.add(node);
        }
        return nodes;
    }

    public static void main(String[] args) throws IOException {
        TreeParser parser = new TreeParser(new FileReader("C:\\work\\projects\\ideah\\src\\haskell\\ask_ghc\\err"));
        List<FakeNode> nodes = parser.readNodes();
        for (FakeNode node : nodes) {
            System.out.println(node.toStruct());
        }
    }
}
