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
import ideah.tree.*;
import ideah.util.*;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;

public final class HaskellFormattingModelBuilder implements FormattingModelBuilder {

    private static final Logger LOG = Logger.getInstance("ideah.formatter.HaskellFormattingModelBuilder");

    @NotNull
    public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
        Block root = null;
        try {
            root = doCreateModel(element);
        } catch (Exception ex) {
            LOG.error(ex);
        }
        PsiFile file = element.getContainingFile();
        if (root == null) {
            root = new FakeBlock(file.getTextRange());
        }
        return new DocumentBasedFormattingModel(root, file.getProject(), settings, file.getFileType(), file);
    }

    private static Block doCreateModel(PsiElement element) throws IOException, InterruptedException, NoMatchException {
        final PsiFile file = element.getContainingFile();
        VirtualFile virtualFile = file.getVirtualFile();
        if (virtualFile == null)
            return null;
        Module module = DeclarationPosition.getDeclModule(file);
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return null;
        List<String> args = new ArrayList<String>();
        args.add(compiler.exe);
        AskUtil.addGhcOptions(module, args);
        args.addAll(Arrays.asList(
            "-m", "ParseTree",
            "-g", compiler.libPath,
            virtualFile.getPath()
        ));
        ProcessLauncher launcher = new ProcessLauncher(false, virtualFile.getInputStream(), args);
        String stdOut = launcher.getStdOut();
        if (stdOut.trim().isEmpty())
            return null;
        RangeFactory factory = new RangeFactory() {

            public IRange parse(String str) {
                LineColRange range = new LineColRange(str);
                return new MyRange(range.getRange(file));
            }

            public IRange create(ILocation start, ILocation end) {
                MyLocation from = (MyLocation) start;
                MyLocation to = (MyLocation) end;
                return MyLocation.create(from, to);
            }
        };
        TreeParser parser = new TreeParser(new BufferedReader(new StringReader(stdOut)), factory);
        ModuleTree moduleTree = parser.readTree(new MyRange(file.getTextRange()));

        SortedMap<ILocation, Filler> ranges = new TreeMap<ILocation, Filler>();
        String text = file.getText();
        HaskellLexer lexer = new HaskellLexer();
        lexer.start(text);
        while (true) {
            IElementType type = lexer.getTokenType();
            if (type == null)
                break;
            if (!HaskellTokenTypes.WHITESPACES.contains(type)) {
                TextRange textRange = new TextRange(lexer.getTokenStart(), lexer.getTokenEnd());
                IRange range = new MyRange(textRange);
                ranges.put(range.getStart(), new Filler(range, type, lexer.getTokenText()));
            }
            lexer.advance();
        }
        moduleTree.buildBlocks(ranges, factory);

        return moduleTree;
    }

    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
