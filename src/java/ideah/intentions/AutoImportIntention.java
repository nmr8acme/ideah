package ideah.intentions;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.codeInspection.HintAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.util.IncorrectOperationException;
import ideah.psi.impl.HPIdentImpl;
import ideah.util.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;

public final class AutoImportIntention implements HintAction {

    private static final Logger LOG = Logger.getInstance("ideah.intentions.AutoImportIntention");

    private final PsiFile psiFile;
    private final TextRange range;
    private final String[] modules;
    private final String importFunction;
    private volatile boolean fixed = false;

    public AutoImportIntention(PsiFile psiFile, TextRange range, String[] modules, String importFunction) {
        this.psiFile = psiFile;
        this.range = range;
        this.modules = modules;
        this.importFunction = importFunction;
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

    private void addImport(final Editor editor, final String moduleToInsert) {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                LineCol insertPos = getInsertPos(moduleToInsert);
                if (insertPos == null)
                    return;
                int offset = insertPos.getOffset(psiFile);
                String string = insertPos.column == 1
                    ? "import " + moduleToInsert + "\n"
                    : ", " + importFunction;
                editor.getDocument().insertString(offset, string);
                fixed = true;
            }
        });
    }

    @Nullable
    private LineCol getInsertPos(String moduleToInsert) {
        CompilerLocation compiler = CompilerLocation.get(DeclarationPosition.getDeclModule(psiFile));
        if (compiler == null)
            return null; // todo
        List<String> args = compiler.getCompileOptionsList(
            "-m", "ImportEnd",
            "-n", moduleToInsert,
            psiFile.getVirtualFile().getPath()
        );
        try {
            ProcessLauncher launcher = new ProcessLauncher(true, null, args);
            BufferedReader bf = new BufferedReader(new StringReader(launcher.getStdOut()));
            while (true) {
                String srcLineCol = bf.readLine();
                if (srcLineCol == null)
                    break;
                LineCol refLineCol = LineCol.parse(srcLineCol);
                if (refLineCol != null)
                    return refLineCol;
            }
        } catch (Exception e) {
            LOG.error(e.getMessage());
        }
        return null;
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
                final Project project = DeclarationPosition.getDeclModule(psiFile).getProject();
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
