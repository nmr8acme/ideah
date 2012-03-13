package ideah.psi.api.util;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFileFactory;
import ideah.HaskellFileType;
import ideah.parser.HaskellFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class HaskellPsiElementFactoryImpl extends HaskellPsiElementFactory {

    private final Project myProject;

    public HaskellPsiElementFactoryImpl(Project project) {
        myProject = project;
    }

    private static final String DUMMY = "DUMMY.";

    @Nullable
    public ASTNode createIdentNodeFromText(@NotNull String newName) {
        HaskellFile dummyFile = createHaskellFileFromText(newName);
        PsiElement firstChild = dummyFile.getFirstChild();
        if (firstChild != null)
            return firstChild.getNode();
        return null;
    }

    @Override
    public boolean hasSyntacticalErrors(@NotNull String text) {
        HaskellFile clojureFile = (HaskellFile) PsiFileFactory.getInstance(getProject()).createFileFromText(DUMMY + HaskellFileType.INSTANCE.getDefaultExtension(), text);
        return hasErrorElement(clojureFile);
    }

    private static boolean hasErrorElement(PsiElement element) {
        if (element instanceof PsiErrorElement) return true;
        for (PsiElement child : element.getChildren()) {
            if (hasErrorElement(child)) return true;
        }
        return false;
    }

    private HaskellFile createHaskellFileFromText(String text) {
        return (HaskellFile) PsiFileFactory.getInstance(getProject()).createFileFromText(DUMMY + HaskellFileType.INSTANCE.getDefaultExtension(), text);
    }

    public Project getProject() {
        return myProject;
    }
}
