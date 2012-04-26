package ideah.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import ideah.lexer.HaskellTokenTypes;
import org.jetbrains.annotations.NotNull;

public final class HaskellParser implements PsiParser, HaskellElementTypes {

    @NotNull
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        builder.setDebugMode(true);
        PsiBuilder.Marker start = builder.mark();
        while (true) {
            IElementType type = builder.getTokenType();
            if (type == null)
                break;
            if (!parseInfixPrefixIdent(builder, HaskellTokenTypes.L_PAREN, HaskellTokenTypes.R_PAREN, HaskellTokenTypes.OPERATORS)
                && !parseInfixPrefixIdent(builder, HaskellTokenTypes.BACKQUOTE, HaskellTokenTypes.BACKQUOTE, HaskellTokenTypes.VAR_IDS)
                && HaskellTokenTypes.IDS.contains(type)) {
                    PsiBuilder.Marker idMark = builder.mark();
                    builder.advanceLexer();
                    idMark.done(type);
            } else {
                builder.advanceLexer();
            }
        }
        start.done(root);
        return builder.getTreeBuilt();
    }

    private static boolean parseInfixPrefixIdent(PsiBuilder builder, IElementType leftType, IElementType rightType, TokenSet idsOrOperators) {
        IElementType type = builder.getTokenType();
        if (type == null)
            return false;
        if (type == leftType) {
            PsiBuilder.Marker infixPrefixExpression = builder.mark();
            builder.advanceLexer();
            IElementType idOrOperType = builder.getTokenType();
            if (idsOrOperators.contains(idOrOperType)) {
                PsiBuilder.Marker idOrOper = builder.mark();
                builder.advanceLexer();
                idOrOper.done(idOrOperType);
                IElementType rparType = builder.getTokenType();
                if (rparType == rightType) {
                    builder.advanceLexer();
                    infixPrefixExpression.done(HaskellElementTypes.INFIX_PREFIX_IDENT);
                    return true;
                } else {
                    infixPrefixExpression.rollbackTo();
                    return false;
                }
            } else {
                infixPrefixExpression.rollbackTo();
                return false;
            }
        }
        return false;
    }
}
