package ideah.formatter;

import com.intellij.formatting.Block;
import com.intellij.formatting.FormattingModel;
import com.intellij.formatting.FormattingModelBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.formatter.DocumentBasedFormattingModel;
import com.intellij.psi.tree.IElementType;
import ideah.lexer.HaskellLexer;
import ideah.lexer.HaskellTokenTypes;
import ideah.tree.Located;
import ideah.tree.ModuleTree;
import ideah.tree.NoMatchException;
import ideah.tree.TreeParser;
import ideah.util.*;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

public final class HaskellFormattingModelBuilder implements FormattingModelBuilder {

    private static final Logger LOG = Logger.getInstance("ideah.formatter.HaskellFormattingModelBuilder");

//    private static int prevSetBit(BitSet set, int from) {
//        for (int i = from; i >= 0; i--) {
//            if (set.get(i))
//                return i;
//        }
//        return -1;
//    }
//
//    @NotNull
//    public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
//        PsiFile file = element.getContainingFile();
//        Project project = file.getProject();
//        VirtualFile virtualFile = file.getVirtualFile();
//        SortedMap<Integer, Block> map = new TreeMap<Integer, Block>();
//        String text;
//        if (virtualFile != null) {
//            SortedSet<TextRange> functionRanges = new TreeSet<TextRange>(new Comparator<TextRange>() {
//                public int compare(TextRange o1, TextRange o2) {
//                    return o1.getStartOffset() - o2.getStartOffset();
//                }
//            });
//            text = file.getText();
//            HaskellLexer lexer = new HaskellLexer();
//            lexer.start(text);
//            BitSet nonSpace = new BitSet();
//            while (true) {
//                IElementType type = lexer.getTokenType();
//                if (type == null)
//                    break;
//                if (!HaskellTokenTypes.WHITESPACES.contains(type)) {
//                    nonSpace.set(lexer.getTokenStart(), lexer.getTokenEnd());
//                }
//                lexer.advance();
//            }
//            try {
//                // todo: возможно, мы парсим не последнюю версию файла - нужно форсировать сохранение?
//                String path = virtualFile.getPath();
//                String output = new ProcessLauncher(false, null, "D:\\home\\oleg\\ideah\\idea\\ideah\\ideah\\format.exe", path).getStdOut();
//                BufferedReader rdr = new BufferedReader(new StringReader(output));
//                while (true) {
//                    String line = rdr.readLine();
//                    if (line == null)
//                        break;
//                    LineColRange lcRange = new LineColRange(line);
//                    TextRange range = lcRange.getRange(file);
//                    if (range.isEmpty()) {
//                        System.out.println("WTF?");
//                    }
//                    map.put(range.getStartOffset(), new HaskellBlock(range, Collections.<Block>emptyList()));
//                    functionRanges.add(range);
//                }
//            } catch (Exception ex) {
//                LOG.error(ex);
//            }
//            int i = 0;
//            for (TextRange range : functionRanges) {
//                int limit = range.getStartOffset();
//                if (i < limit) {
//                    int from = nonSpace.nextSetBit(i);
//                    if (from >= 0 && from < limit) {
//                        int to = prevSetBit(nonSpace, limit - 1);
//                        if (to >= from) {
//                            TextRange newRange = new TextRange(from, to + 1);
//                            map.put(newRange.getStartOffset(), new HaskellBlock(newRange, Collections.<Block>emptyList()));
//                        }
//                    }
//                }
//                i = range.getEndOffset();
//            }
//            if (i < nonSpace.length()) {
//                int from = nonSpace.nextSetBit(i);
//                if (from >= 0) {
//                    int to = prevSetBit(nonSpace, nonSpace.length() - 1);
//                    if (to >= from) {
//                        TextRange newRange = new TextRange(from, to + 1);
//                        map.put(newRange.getStartOffset(), new HaskellBlock(newRange, Collections.<Block>emptyList()));
//                    }
//                }
//            }
//        } else {
//            text = "";
//        }
//        List<Block> blocks = new ArrayList<Block>(map.values());
//        for (Block block : blocks) {
//            System.out.println(block.getTextRange() + ": '" + block.getTextRange().substring(text) + "'");
//        }
//        Block rootBlock = new HaskellBlock(file.getTextRange(), blocks);
//        return new DocumentBasedFormattingModel(rootBlock, project, settings, file.getFileType(), file);
//    }


    @NotNull
    public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
        FormattingModel model = null;
        try {
            model = doCreateModel(element, settings);
        } catch (Exception ex) {
            LOG.error(ex);
        }
        if (model == null) {
            PsiFile file = element.getContainingFile();
            Block root = new FakeBlock(file.getTextRange());
            return new DocumentBasedFormattingModel(root, file.getProject(), settings, file.getFileType(), file);
        } else {
            return model;
        }
    }

    private static FormattingModel doCreateModel(PsiElement element, CodeStyleSettings settings) throws IOException, InterruptedException, NoMatchException {
        PsiFile file = element.getContainingFile();
        VirtualFile virtualFile = file.getVirtualFile();
        if (virtualFile == null)
            return null;
        Module module = DeclarationPosition.getModule(file);
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return null;
        ProcessLauncher launcher = new ProcessLauncher(
            false, virtualFile.getInputStream(),
            compiler.exe,
            "-m", "ParseTree",
            "-g", compiler.libPath,
            virtualFile.getPath()
        );
        String stdOut = launcher.getStdOut();
        if (stdOut.trim().isEmpty())
            return null;
        TreeParser parser = new TreeParser(new BufferedReader(new StringReader(stdOut)));
        ModuleTree moduleTree = parser.readTree(LineColRange.fromTextRange(file, file.getTextRange()));

        SortedMap<LineCol, LineColRange> ranges = new TreeMap<LineCol, LineColRange>();
        String text = file.getText();
        HaskellLexer lexer = new HaskellLexer();
        lexer.start(text);
        while (true) {
            IElementType type = lexer.getTokenType();
            if (type == null)
                break;
            if (!HaskellTokenTypes.WHITESPACES.contains(type)) {
                TextRange textRange = new TextRange(lexer.getTokenStart(), lexer.getTokenEnd());
                LineColRange range = LineColRange.fromTextRange(file, textRange);
                ranges.put(range.start, range);
            }
            lexer.advance();
        }
        moduleTree.fillGaps(ranges);

        Block root = toBlock(file, moduleTree);

        return new DocumentBasedFormattingModel(root, file.getProject(), settings, file.getFileType(), file);
    }

    private static HaskellBlock toBlock(PsiFile file, Located located) {
        List<Located> children = located.getBlocks();
        List<Block> subBlocks = new ArrayList<Block>();
        for (Located child : children) {
            subBlocks.add(toBlock(file, child));
        }
        return new HaskellBlock(located.location.getRange(file), subBlocks);
    }

    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
