package ideah.repl.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.actionSystem.Presentation;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import ideah.repl.HaskellConsoleRunner;

public class RunHaskellConsoleAction extends AnAction implements DumbAware {

    @Override
    public void update(AnActionEvent e) {
        Module m = getModule(e);
        Presentation presentation = e.getPresentation();
        if (m == null) {
            presentation.setEnabled(false);
            return;
        }
        presentation.setEnabled(true);
        super.update(e);
    }


    @Override
    public void actionPerformed(AnActionEvent event) {
        Module module = getModule(event);
        String path = ModuleRootManager.getInstance(module).getContentRoots()[0].getPath();
        HaskellConsoleRunner.run(module, path);
    }

    static Module getModule(AnActionEvent e) {
        Module module = e.getData(DataKeys.MODULE);
        if (module == null) {
            Project project = e.getData(DataKeys.PROJECT);
            if (project == null)
                return null;
            Module[] modules = ModuleManager.getInstance(project).getModules();
            if (modules.length > 0) {
                module = modules[0];
            }
        }
        return module;
    }
}
