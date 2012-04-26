package ideah.rename;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.rename.RenameDialog;
import ideah.lexer.HaskellTokenType;
import ideah.lexer.HaskellTokenTypes;
import ideah.lexer.LexedIdentifier;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

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
        String oldName = myPsiElement.getText();
        LexedIdentifier newId = LexedIdentifier.parse(newName);
        LexedIdentifier oldId = LexedIdentifier.parse(oldName);
        if (newId == null || oldId == null)
            return;
        HaskellTokenType newType = newId.type;
        HaskellTokenType oldType = oldId.type;
        String createdName = newName;
        boolean newNameOperator = HaskellTokenTypes.OPERATORS.contains(newType);
        if (!newNameOperator == HaskellTokenTypes.OPERATORS.contains(oldType)) {
            createdName = createNewName(newNameOperator, "operator", "variable", oldName, newName, "Identifier type change",
                "No, do not perform rename", new RenameChange() {
                public String changeName() {
                    return null;
                }
            });
        } else if (!newNameOperator) {
            final boolean newNameConstructor = HaskellTokenTypes.CON_ID == newType;
            if (newNameConstructor == (HaskellTokenTypes.VAR_ID == oldType)) {
                createdName = createNewName(newNameConstructor, "constructor", "variable", oldName, newName, "First Letter Case Change",
                    "No, rename without changing case", new RenameChange() {
                    public String changeName() {
                        char c = newName.charAt(0);
                        return newName.replace(c, newNameConstructor ? Character.toLowerCase(c) : Character.toUpperCase(c));
                    }
                });
            }
        }
        if (createdName == null)
            return;
        performRename(createdName);
    }

    private static String createNewName(boolean isNewNameType1, String type1, String type2, String oldName, String newName, String title, String no, RenameChange change) {
        String oldTypeName = isNewNameType1 ? type2 : type1;
        String newTypeName = isNewNameType1 ? type1 : type2;
        int yesNoCancel = Messages.showYesNoCancelDialog("You are about to change the identifier type of '" + oldName + "'.\n\n" +
            "Are you sure you want to change it from " + withArticle(oldTypeName) + " to " + withArticle(newTypeName) + "?", title, "Yes", no,
            "Cancel", Messages.getQuestionIcon());
        switch (yesNoCancel) {
        case Messages.YES: return newName;
        case Messages.NO: return change.changeName();
        default: return null;
        }
    }

    private static String withArticle(String name) {
        Set<Character> vowels = new HashSet<Character>(5);
        vowels.addAll(Arrays.asList('a', 'e', 'i', 'o', 'u'));
        return "a" + (vowels.contains(Character.toLowerCase(name.charAt(0))) ? "n " : " ") + name;
    }

    private interface RenameChange {
        String changeName();
    }
}