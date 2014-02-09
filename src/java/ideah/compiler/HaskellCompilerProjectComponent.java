package ideah.compiler;

import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.components.AbstractProjectComponent;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import ideah.HaskellFileType;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.HashSet;

public final class HaskellCompilerProjectComponent extends AbstractProjectComponent {

    public HaskellCompilerProjectComponent(Project project) {
        super(project);
    }

    public void projectOpened() {
        CompilerManager manager = CompilerManager.getInstance(myProject);
        for (HaskellCompiler compiler : manager.getCompilers(HaskellCompiler.class)) {
            manager.removeCompiler(compiler);
        }
        HashSet<FileType> inputSet = new HashSet<FileType>(Arrays.asList(HaskellFileType.INSTANCE));
        HashSet<FileType> outputSet = new HashSet<FileType>(Arrays.asList(HiFileType.INSTANCE));
        manager.addTranslatingCompiler(new HaskellCompiler(myProject), inputSet, outputSet);
    }

    @NotNull
    public String getComponentName() {
        return "HaskellCompilerComponent";
    }
}
