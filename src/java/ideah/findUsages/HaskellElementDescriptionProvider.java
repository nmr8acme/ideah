package ideah.findUsages;

import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import ideah.psi.impl.HPIdentImpl;
import org.jetbrains.annotations.NotNull;

public class HaskellElementDescriptionProvider implements ElementDescriptionProvider {

    public String getElementDescription(@NotNull PsiElement psiElement, @NotNull ElementDescriptionLocation elementDescriptionLocation) {
        if (psiElement instanceof HPIdentImpl) {
            return psiElement.getText();
        }
        return null;
    }
}
