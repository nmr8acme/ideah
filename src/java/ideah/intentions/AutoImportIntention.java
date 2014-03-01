package ideah.intentions;

import com.intellij.codeInspection.HintAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

public final class AutoImportIntention implements HintAction {

    @NotNull
    public String getText() {
        return "Import symbol";
    }

    @NotNull
    public String getFamilyName() {
        return "Auto import";
    }

    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile psiFile) {
        return true; // todo
    }

    public void invoke(@NotNull Project project, final Editor editor, PsiFile psiFile) throws IncorrectOperationException {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                editor.getDocument().insertString(0, "import Data.List\n"); // todo
            }
        });
    }

    public boolean startInWriteAction() {
        return false;
    }

    public boolean showHint(@NotNull Editor editor) {
        return true;
    }
}
