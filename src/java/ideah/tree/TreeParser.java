package ideah.tree;

import ideah.util.LineColRange;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
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

    public ModuleTree readTree(LineColRange location) throws NoMatchException, IOException {
        List<FakeNode> nodes = readNodes();
        return nodes.get(0).toModule(location);
    }

    public static void main(String[] args) throws IOException, NoMatchException {
        TreeParser parser = new TreeParser(new FileReader("C:\\work\\projects\\ideah\\src\\haskell\\ask_ghc\\err"));
        ModuleTree module = parser.readTree(LineColRange.getFake());
        System.out.println(module);
    }
}
