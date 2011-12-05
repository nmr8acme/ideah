package ideah.repl;

import com.intellij.execution.console.LanguageConsoleViewImpl;
import com.intellij.execution.process.ConsoleHistoryModel;
import com.intellij.openapi.project.Project;

public final class HaskellConsoleView extends LanguageConsoleViewImpl {

    HaskellConsoleView(Project project,
                       String title,
                       ConsoleHistoryModel historyModel) {
        super(project, new HaskellConsole(project, title, historyModel));
    }

    @Override
    public HaskellConsole getConsole() {
        return (HaskellConsole) super.getConsole();
    }
}
