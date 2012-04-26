package ideah.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import ideah.HaskellFileType;
import ideah.lexer.HaskellLexer;
import ideah.lexer.HaskellTokenTypes;
import ideah.psi.impl.HPIdentImpl;
import ideah.psi.impl.HPInfixPrefixIdentImpl;
import ideah.psi.impl.HPModuleImpl;
import ideah.psi.impl.HPOtherImpl;
import org.jetbrains.annotations.NotNull;

public final class HaskellParserDefinition implements ParserDefinition, HaskellTokenTypes {

    public static final IFileElementType HASKELL_FILE = new IFileElementType(HaskellFileType.HASKELL_LANGUAGE);

    @NotNull
    public Lexer createLexer(Project project) {
        return new HaskellLexer();
    }

    public PsiParser createParser(Project project) {
        return new HaskellParser();
    }

    public IFileElementType getFileNodeType() {
        return HASKELL_FILE;
    }

    @NotNull
    public TokenSet getWhitespaceTokens() {
        return WHITESPACES;
    }

    @NotNull
    public TokenSet getCommentTokens() {
        return COMMENTS;
    }

    @NotNull
    public TokenSet getStringLiteralElements() {
        return STRINGS;
    }

    @NotNull
    public PsiElement createElement(ASTNode node) {
        IElementType type = node.getElementType();
        if (type == HaskellElementTypes.MODULE) { // where is it initialized???
            return new HPModuleImpl(node);
        } else if (type == HaskellElementTypes.INFIX_PREFIX_IDENT) {
            return new HPInfixPrefixIdentImpl(node);
        } else if (HaskellTokenTypes.IDS.contains(type)) {
            return new HPIdentImpl(node);
        } else {
            return new HPOtherImpl(node);
        }
    }

    public PsiFile createFile(FileViewProvider viewProvider) {
        return new HaskellFileImpl(viewProvider);
    }

    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
        return SpaceRequirements.MAY;
    }
}
