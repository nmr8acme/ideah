package ideah.util;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileSystemItem;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class DeclarationPosition {

    public final LineCol coord;
    public final String module;

    public DeclarationPosition(LineCol coord, String module) {
        this.coord = coord;
        this.module = module;
    }

    @Nullable
    public static DeclarationPosition get(@NotNull PsiFile psiFile, @Nullable LineCol coord) throws IOException, InterruptedException {
        if (coord == null)
            return null;
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        Module module = getDeclModule(psiFile);
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return null;
        String sourcePath = GHCUtil.rootsAsString(module, false);
        List<String> args = new ArrayList<String>();
        args.add(compiler.exe);
        AskUtil.addGhcOptions(module, args);
        args.addAll(Arrays.asList(
            "-m", "GetDeclPos",
            "-g", compiler.libPath,
            "-s", sourcePath,
            "--line-number", String.valueOf(coord.line), "--column-number", String.valueOf(coord.column),
            file.getPath()
        ));
        ProcessLauncher launcher = new ProcessLauncher(false, null, args);
        BufferedReader reader = new BufferedReader(new StringReader(launcher.getStdOut()));
        String lineCol = reader.readLine();
        String moduleName = reader.readLine();
        if (lineCol != null && moduleName != null) {
            LineCol declCoord = LineCol.parse(lineCol);
            if (declCoord == null)
                return null;
            return new DeclarationPosition(declCoord, moduleName);
        } else {
            return null;
        }
    }

    public static Module getDeclModule(PsiFile psiFile) {
        return getDeclModule(psiFile.getProject(), psiFile);
    }

    public static Module getDeclModule(Project project, PsiFileSystemItem psiFile) {
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        return getModule(project, file);
    }

    public static Module getModule(Project project, VirtualFile file) {
        return ProjectRootManager.getInstance(project).getFileIndex().getModuleForFile(file);
    }
}
