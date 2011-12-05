package ideah.repl;

import com.intellij.codeInsight.lookup.Lookup;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.EmptyAction;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.util.IconLoader;

import javax.swing.*;

abstract class HaskellExecuteActionBase extends DumbAwareAction {

    static final Icon ICON = IconLoader.getIcon("/ideah/haskell_16x16.png"); // todo: other icon?

    protected final HaskellConsole myLanguageConsole;
    protected final ProcessHandler myProcessHandler;
    protected final HaskellConsoleExecuteActionHandler myConsoleExecuteActionHandler;

    protected HaskellExecuteActionBase(HaskellConsole languageConsole,
                                       ProcessHandler processHandler,
                                       HaskellConsoleExecuteActionHandler consoleExecuteActionHandler,
                                       String actionId) {
        super(null, null, ICON);
        myLanguageConsole = languageConsole;
        myProcessHandler = processHandler;
        myConsoleExecuteActionHandler = consoleExecuteActionHandler;
        //EmptyAction.setupAction(this, actionId, null); // todo
    }

    public void update(AnActionEvent e) {
        EditorEx editor = myLanguageConsole.getConsoleEditor();
        Lookup lookup = LookupManager.getActiveLookup(editor);
        e.getPresentation().setEnabled(!myProcessHandler.isProcessTerminated() &&
            (lookup == null || !lookup.isCompletion()));
    }

    HaskellConsoleExecuteActionHandler getExecuteActionHandler() {
        return myConsoleExecuteActionHandler;
    }
}
