package ideah.repl;

import com.intellij.execution.console.LanguageConsoleImpl;
import com.intellij.openapi.project.Project;
import ideah.HaskellFileType;

final class HaskellConsole extends LanguageConsoleImpl {

    HaskellConsole(Project project, String title) {
        super(project, title, HaskellFileType.HASKELL_LANGUAGE);
    }
}
