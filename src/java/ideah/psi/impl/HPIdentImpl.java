package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.IElementType;
import ideah.lexer.HaskellTokenType;
import ideah.parser.HaskellElementTypes;
import ideah.psi.api.HPIdent;
import org.jetbrains.annotations.NotNull;

public final class HPIdentImpl extends HaskellAbstractIdentImpl implements HPIdent {

    public HPIdentImpl(@NotNull ASTNode node) {
        super(node);
    }

    @Override
    protected ASTNode getNodeToBeReplaced() {
        return isPrefixInfixIdent() ? getParent().getNode() : getNode();
    }

    @Override
    protected ASTNode getNodeToBeInsertedTo() {
        ASTNode parent = getParent().getNode();
        return isPrefixInfixIdent() ? parent.getTreeParent() : parent;
    }

    @Override
    protected boolean isPrefixInfixIdent() {
        ASTNode parentNode = getParent().getNode();
        ASTNode grandParentNode = parentNode.getTreeParent();
        HaskellTokenType infixPrefixIdentType = HaskellElementTypes.INFIX_PREFIX_IDENT;
        IElementType parentType = grandParentNode.getElementType() == infixPrefixIdentType ? infixPrefixIdentType : parentNode.getElementType();
        return parentType == infixPrefixIdentType;
    }
}
