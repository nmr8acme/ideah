package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import ideah.psi.api.HPInfixPrefixIdent;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public final class HPInfixPrefixIdentImpl extends HaskellAbstractIdentImpl implements HPInfixPrefixIdent {

    public HPInfixPrefixIdentImpl(@NotNull ASTNode node) {
        super(node);
    }

    @Override
    protected ASTNode getNodeToBeReplaced() {
        return getNode();
    }

    @Override
    protected ASTNode getNodeToBeInsertedTo() {
        return getParent().getNode();
    }

    @Override
    protected boolean isPrefixInfixIdent() {
        return true;
    }
}
