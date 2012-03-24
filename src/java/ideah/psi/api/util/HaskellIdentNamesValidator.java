package ideah.psi.api.util;

import com.intellij.lang.refactoring.NamesValidator;
import com.intellij.openapi.project.Project;
import ideah.lexer.HaskellLexer;

public final class HaskellIdentNamesValidator implements NamesValidator {

    public boolean isKeyword(String name, Project project) {
        return HaskellLexer.getKeywords().contains(name);
    }

    public boolean isIdentifier(String name, Project project) { // todo: use lexer
        if (!Character.isJavaIdentifierStart(name.charAt(0)))
            return false;
        for (int i = 1; i < name.length(); i++) {
            char c = name.charAt(i);
            if (!(Character.isLetterOrDigit(c) || c == '_' || c == '\''))
                return false;
        }
        return !isKeyword(name, project);
    }
}
