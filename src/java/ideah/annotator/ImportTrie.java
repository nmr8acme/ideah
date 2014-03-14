package ideah.annotator;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import ideah.util.CompilerLocation;
import ideah.util.ProcessLauncher;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;

class ImportTrie {

    private static final Logger LOG = Logger.getInstance("ideah.annotator.ImportTrie");

    private final Node rootNode = new Node(0, new HashMap<String, Node>());

    private static final int LOCAL_WEIGHT = 2;
    private static final int EXTERNAL_WEIGHT = 1;

    private static final ImportTrie EMPTY = new ImportTrie(new TreeSet<String>(), new TreeSet<String>());

    private ImportTrie(SortedSet<String> localImports, SortedSet<String> externalImports) {
        for (String imp : localImports) {
            insertImport(rootNode, imp, LOCAL_WEIGHT);
        }
        for (String imp : externalImports) {
            insertImport(rootNode, imp, EXTERNAL_WEIGHT);
        }
    }

    static ImportTrie get(Module module, String path, List<String> otherFiles) {
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return null;
        List<String> args = compiler.getCompileOptionsList(
            "-m", "AllImportsWithPkgs",
            path
        );
        args.addAll(otherFiles);
        try {
            ProcessLauncher launcher = new ProcessLauncher(false, null, args);
            String stdOut = launcher.getStdOut();
            SortedSet<String> localImports = new TreeSet<String>();
            SortedSet<String> externalImports = new TreeSet<String>();
            BufferedReader rdr = new BufferedReader(new StringReader(stdOut));
            parseLocals(localImports, rdr);
            parseLocals(externalImports, rdr);
            return new ImportTrie(localImports, externalImports);
        } catch (Exception ex) {
            LOG.error(ex);
        }
        return EMPTY;
    }

    private static void parseLocals(SortedSet<String> imports, BufferedReader rdr) throws IOException {
        while (true) {
            String line = rdr.readLine();
            if (line == null || ProcessLauncher.NEW_MSG_INDICATOR.equals(line))
                break;
            imports.add(line.trim());
        }
    }

    private void insertImport(Node node, String imp, int weight) {
        if (imp == null)
            return;
        int dot = imp.indexOf('.');
        if (dot >= 0 && dot < imp.length()) {
            String prefix = imp.substring(0, dot);
            String suffix = imp.substring(dot + 1);
            insertImportPrefixAndSuffix(node, suffix, prefix, weight);
        } else {
            insertImportPrefixAndSuffix(node, null, imp, weight);
        }
    }

    private void insertImportPrefixAndSuffix(Node node, String suffix, String key, int weight) {
        Node target = node.subtrie.get(key);
        if (target == null) {
            Node newTarget = new Node(weight, new HashMap<String, Node>());
            insertImport(newTarget, suffix, weight);
            node.subtrie.put(key, newTarget);
        } else {
            target.score += weight;
            insertImport(target, suffix, weight);
        }
    }

    double getScore(String importName) {
        String[] suffixes = importName.split("\\.");
        double score = 0;
        Node node = rootNode;
        for (String imp : suffixes) {
            node = node.subtrie.get(imp);
            if (node == null)
                break;
            score += node.score;
        }
        return score / suffixes.length;
    }

    private class Node {
        int score = 0;
        final Map<String, Node> subtrie;

        private Node(int score, Map<String, Node> subtrie) {
            this.score = score;
            this.subtrie = subtrie;
        }


    }
}
