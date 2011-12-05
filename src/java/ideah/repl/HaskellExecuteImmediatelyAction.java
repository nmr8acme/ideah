package ideah.repl;

import com.intellij.execution.process.ProcessHandler;
import com.intellij.openapi.actionSystem.AnActionEvent;

final class HaskellExecuteImmediatelyAction extends HaskellExecuteActionBase {

    HaskellExecuteImmediatelyAction(HaskellConsole languageConsole, ProcessHandler processHandler, HaskellConsoleExecuteActionHandler consoleExecuteActionHandler) {
        super(languageConsole, processHandler, consoleExecuteActionHandler, HaskellConsoleRunner.EXECUTE_ACTION_IMMEDIATELY_ID);
    }

    public void actionPerformed(AnActionEvent e) {
        getExecuteActionHandler().runExecuteAction(myLanguageConsole, true);
    }
}
