package ideah.psi.api.util;

import com.intellij.lang.refactoring.NamesValidator;
import com.intellij.openapi.project.Project;
import ideah.lexer.HaskellLexer;

import java.lang.String;

public class HaskellIdentNamesValidator implements NamesValidator {

    public boolean isKeyword(final String name, final Project project) {
        for (String s : HaskellLexer.getKeywords()) {
            if (name.equals(s))
                return true;
        }
        return false;
    }

    public boolean isIdentifier(final String name, final Project project) {
        return !isKeyword(name, project); // todo: valid identifier name check
    }
}
