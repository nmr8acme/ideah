package ideah.repl;

import com.intellij.execution.console.LanguageConsoleViewImpl;
import com.intellij.openapi.project.Project;
import ideah.HaskellFileType;

final class HaskellConsoleView extends LanguageConsoleViewImpl {

    HaskellConsoleView(Project project, String title) {
        super(project, title, HaskellFileType.HASKELL_LANGUAGE);
    }

    @Override
    public HaskellConsole getConsole() {
        return (HaskellConsole) super.getConsole();
    }
}
