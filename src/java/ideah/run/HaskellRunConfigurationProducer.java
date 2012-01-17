package ideah.run;

import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import ideah.parser.HaskellFile;
import ideah.util.CompilerLocation;
import ideah.util.ProcessLauncher;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.List;

public final class HaskellRunConfigurationProducer extends RuntimeConfigurationProducer {

    private HaskellFile runFile;

    private static final Logger LOG = Logger.getInstance("ideah.run.HaskellRunConfigurationProducer");

    public HaskellRunConfigurationProducer() {
        super(HaskellRunConfigurationType.INSTANCE);
    }

    public PsiElement getSourceElement() {
        return runFile;
    }

    protected RunnerAndConfigurationSettings createConfigurationByElement(com.intellij.execution.Location location, ConfigurationContext context) {
        PsiFile file = location.getPsiElement().getContainingFile();
        if (!(file instanceof HaskellFile))
            return null;
        HaskellFile hsFile = (HaskellFile) file;
        try {
            VirtualFile virtualFile = file.getVirtualFile();
            if (virtualFile == null)
                return null;
            if (!hasMain(virtualFile.getPath(), context.getModule()))
                return null;
            runFile = hsFile;
            Project project = file.getProject();
            RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(project, context);
            HaskellRunConfiguration configuration = (HaskellRunConfiguration) settings.getConfiguration();
            configuration.setMainFile(runFile);
            VirtualFile baseDir = project.getBaseDir();
            if (baseDir != null) {
                configuration.setWorkingDirectory(baseDir.getPath());
            }
            configuration.setName(configuration.getGeneratedName());
            return settings;
        } catch (Exception ex) {
            LOG.error(ex);
        }
        return null;
    }

    static boolean hasMain(String file, Module module) throws IOException, InterruptedException {
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null) {
            return false;
        }
        List<String> args = compiler.getCompileOptionsList(
            "-m", "CheckMain",
            file
        );
        ProcessLauncher launcher = new ProcessLauncher(false, new FileInputStream(file), args);
        String stdOut = launcher.getStdOut();
        return stdOut != null && stdOut.contains("t");
    }

    public int compareTo(Object o) {
        return PREFERED;
    }
}
