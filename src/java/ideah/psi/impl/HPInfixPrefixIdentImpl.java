package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import ideah.psi.api.HPInfixPrefixIdent;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public final class HPInfixPrefixIdentImpl extends HaskellAbstractIdentImpl implements HPInfixPrefixIdent {

    private final HPIdentImpl middleElement;

    public HPInfixPrefixIdentImpl(@NotNull ASTNode node) {
        super(node);
        PsiElement firstChild = getFirstChild();
        assert firstChild != null;
        PsiElement nextSibling = firstChild.getNextSibling();
        assert nextSibling instanceof HPIdentImpl;
        middleElement = (HPIdentImpl) nextSibling;
    }

    @Override
    public PsiElement setName(@NonNls @NotNull String name) throws IncorrectOperationException {
        return middleElement.setName(name);
    }
}
