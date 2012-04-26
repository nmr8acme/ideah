package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import ideah.psi.api.HPInfixPrefixIdent;
import org.jetbrains.annotations.NotNull;

public final class HPInfixPrefixIdentImpl extends HPIdentImpl implements HPInfixPrefixIdent {

    public HPInfixPrefixIdentImpl(@NotNull ASTNode node) {
        super(node);
    }

    public String toString() {
        PsiElement firstChild = getFirstChild();
        if (firstChild == null)
            return null;
        ASTNode left = firstChild.getNode();
        ASTNode middle = left.getTreeNext();
        if (middle == null)
            return null;
        ASTNode right = middle.getTreeNext();
        if (right == null)
            return null;
        return left.toString() + middle.toString() + right.toString();
    }
}
