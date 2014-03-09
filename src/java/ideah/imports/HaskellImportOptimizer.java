package ideah.imports;

import com.intellij.lang.ImportOptimizer;
import com.intellij.psi.PsiFile;
import ideah.parser.HaskellFile;
import org.jetbrains.annotations.NotNull;

public class HaskellImportOptimizer implements ImportOptimizer {

    @Override
    public boolean supports(PsiFile psiFile) {
        return psiFile instanceof HaskellFile;
    }

    @NotNull
    @Override
    public Runnable processFile(PsiFile psiFile) {
//        if (message.matches("The (qualified )?import of .+ is redundant.*")) {
//            Module module = DeclarationPosition.getDeclModule(psiFile);
//        } else if (message.matches("The import of .+ from .+ is redundant\n")) {
//            Module module = DeclarationPosition.getDeclModule(psiFile);
//        }
        return null;
    }
}
