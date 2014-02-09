package ideah.run;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import ideah.parser.HaskellFile;
import ideah.util.CompilerLocation;
import ideah.util.ProcessLauncher;

import java.io.IOException;
import java.util.List;

public final class HaskellRunConfigurationProducer extends RunConfigurationProducer<HaskellRunConfiguration> {

    private HaskellFile runFile;

    private static final Logger LOG = Logger.getInstance("ideah.run.HaskellRunConfigurationProducer");

    public HaskellRunConfigurationProducer() {
        super(HaskellRunConfigurationType.INSTANCE);
    }

    public PsiElement getSourceElement() {
        return runFile;
    }

    protected boolean setupConfigurationFromContext(HaskellRunConfiguration configuration, ConfigurationContext context, Ref<PsiElement> sourceElement) {
        PsiElement psiElement = sourceElement.get();
        PsiFile file = psiElement.getContainingFile();
        if (!(file instanceof HaskellFile))
            return false;
        HaskellFile hsFile = (HaskellFile) file;
        try {
            VirtualFile virtualFile = file.getVirtualFile();
            if (virtualFile == null)
                return false;
            if (!hasMain(virtualFile, context.getModule()))
                return false;
            runFile = hsFile;
            Project project = file.getProject();
            configuration.setMainFile(runFile);
            VirtualFile baseDir = project.getBaseDir();
            if (baseDir != null) {
                configuration.setWorkingDirectory(baseDir.getPath());
            }
            configuration.setName(configuration.suggestedName());
            return true;
        } catch (Exception ex) {
            LOG.error(ex);
        }
        return false;
    }

    public boolean isConfigurationFromContext(HaskellRunConfiguration configuration, ConfigurationContext context) {
        return false; // todo
    }

//    protected RunnerAndConfigurationSettings createConfigurationByElement(Location location, ConfigurationContext context) {
//        PsiFile file = location.getPsiElement().getContainingFile();
//        if (!(file instanceof HaskellFile))
//            return null;
//        HaskellFile hsFile = (HaskellFile) file;
//        try {
//            VirtualFile virtualFile = file.getVirtualFile();
//            if (virtualFile == null)
//                return null;
//            if (!hasMain(virtualFile, context.getModule()))
//                return null;
//            runFile = hsFile;
//            Project project = file.getProject();
//            RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(project, context);
//            HaskellRunConfiguration configuration = (HaskellRunConfiguration) settings.getConfiguration();
//            configuration.setMainFile(runFile);
//            VirtualFile baseDir = project.getBaseDir();
//            if (baseDir != null) {
//                configuration.setWorkingDirectory(baseDir.getPath());
//            }
//            configuration.setName(configuration.suggestedName());
//            return settings;
//        } catch (Exception ex) {
//            LOG.error(ex);
//        }
//        return null;
//    }

    static boolean hasMain(VirtualFile file, Module module) throws IOException, InterruptedException {
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null) {
            return false;
        }
        List<String> args = compiler.getCompileOptionsList(
            "-m", "CheckMain",
            file.getPath()
        );
        ProcessLauncher launcher = new ProcessLauncher(false, file.getInputStream(), args);
        String stdOut = launcher.getStdOut();
        return stdOut != null && stdOut.contains("t");
    }

//    public int compareTo(Object o) {
//        return PREFERED;
//    }
}
