package ideah.intentions;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.codeInspection.HintAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

public final class AutoImportIntention implements HintAction {

    private final Project project;
    private final TextRange range;
    private final String[] modules;
    private volatile boolean fixed = false;

    public AutoImportIntention(Project project, TextRange range, String[] modules) {
        this.project = project;
        this.range = range;
        this.modules = modules;
    }

    @NotNull
    public String getText() {
        return "Import symbol";
    }

    @NotNull
    public String getFamilyName() {
        return "Auto import";
    }

    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile psiFile) {
        return !fixed;
    }

    public void invoke(@NotNull Project project, final Editor editor, PsiFile psiFile) throws IncorrectOperationException {
        createAddImportAction(editor).execute();
    }

    private void addImport(final Editor editor, final String what) {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                editor.getDocument().insertString(0, "import " + what + "\n"); // todo: choose place
                fixed = true;
            }
        });
    }

    public boolean startInWriteAction() {
        return false;
    }

    public boolean showHint(@NotNull final Editor editor) {
        HintManager.getInstance().showQuestionHint(editor, "Import symbol", range.getStartOffset(), range.getEndOffset(), createAddImportAction(editor));
        return true;
    }

    private QuestionAction createAddImportAction(final Editor editor) {
        return new QuestionAction() {
            public boolean execute() {
                PsiDocumentManager.getInstance(project).commitAllDocuments();
                final BaseListPopupStep<String> step = new BaseListPopupStep<String>("Module to import", modules) {

                    public boolean isAutoSelectionEnabled() {
                        return false;
                    }

                    public boolean isSpeedSearchEnabled() {
                        return true;
                    }

                    public PopupStep onChosen(String selectedValue, boolean finalChoice) {
                        if (finalChoice && selectedValue != null) {
                            PsiDocumentManager.getInstance(project).commitAllDocuments();
                            addImport(editor, selectedValue);
                        }
                        return FINAL_CHOICE;
                    }

                    public boolean hasSubstep(String selectedValue) {
                        return false;
                    }

                    public String getTextFor(String value) {
                        return value;
                    }
                };
                JBPopupFactory.getInstance().createListPopup(step).showInBestPositionFor(editor);
                return true;
            }
        };
    }
}
