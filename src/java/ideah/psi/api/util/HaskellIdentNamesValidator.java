package ideah.psi.api.util;

import com.intellij.lang.refactoring.NamesValidator;
import com.intellij.openapi.project.Project;
import ideah.lexer.HaskellLexer;
import ideah.lexer.HaskellTokenTypes;
import ideah.lexer.LexedIdentifier;

public final class HaskellIdentNamesValidator implements NamesValidator {

    public boolean isKeyword(String name, Project project) {
        return HaskellLexer.getKeywords().contains(name);
    }

    public boolean isIdentifier(String name, Project project) {
        LexedIdentifier identifier = LexedIdentifier.parse(name);
        if (identifier == null)
            return false;
        return HaskellTokenTypes.IDS.contains(identifier.type) && !isKeyword(name, project);
    }
}
