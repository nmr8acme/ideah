package ideah.rename;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.rename.RenameDialog;

final class HaskellRenameDialog extends RenameDialog {

    private static final Logger LOG = Logger.getInstance("ideah.rename.HaskellRenameDialog");
    
    private final PsiElement myPsiElement;
    
    HaskellRenameDialog(Project project, PsiElement element, PsiElement nameSuggestionContext, Editor editor) {
        super(project, element, nameSuggestionContext, editor);
        myPsiElement = element;
    }

    @Override
    protected void doAction() {
        LOG.assertTrue(myPsiElement.isValid());
        final String newName = getNewName();
        char c = newName.charAt(0);
        boolean newNameUpper = Character.isUpperCase(c);
        String oldName = myPsiElement.getText();
        if (newNameUpper != Character.isUpperCase(oldName.charAt(0))) {
            String variable = "variable";
            String constructor = "constructor";
            String oldType = newNameUpper ? variable : constructor;
            String newType = newNameUpper ? constructor : variable;
            int yesNoCancel = Messages.showYesNoCancelDialog("You are about to change the identifier type of '" + oldName + "'.\n\n" +
                "Are you sure you want to change it from a " + oldType + " to a " + newType + "?", "Identifier Type Change", "Yes",
                "No, refactor without changing case", "Cancel", Messages.getQuestionIcon());
            switch (yesNoCancel) {
                case Messages.YES: performRename(newName); return;
                case Messages.NO: performRename(newName.replace(c, newNameUpper ? Character.toLowerCase(c) : Character.toUpperCase(c))); return;
                default: return;
            }
        }
        performRename(newName);
    }
}
