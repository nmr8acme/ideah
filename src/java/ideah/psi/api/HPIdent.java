package ideah.psi.api;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import ideah.psi.api.util.HaskellPsiElement;
import org.jetbrains.annotations.Nullable;

public interface HPIdent extends HaskellPsiElement, PsiNamedElement {

    @Nullable
    PsiElement getReferenceNameElement();
}
