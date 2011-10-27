package ideah.util;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileSystemItem;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

public final class DeclarationPosition {

    public final LineCol coord;
    public final String module;

    private DeclarationPosition(LineCol coord, String module) {
        this.coord = coord;
        this.module = module;
    }

    public static DeclarationPosition get(PsiFile psiFile, LineCol coord) throws IOException, InterruptedException {
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        Module module = getModule(psiFile);
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return null;
        String sourcePath = CompilerLocation.rootsAsString(module, false);
        ProcessLauncher launcher = new ProcessLauncher(
            false, null,
            compiler.exe,
            "-m", "GetDeclPos",
            "-g", compiler.libPath,
            "-s", sourcePath,
            "--line-number", String.valueOf(coord.line), "--column-number", String.valueOf(coord.column),
            file.getPath()
        );
        BufferedReader reader = new BufferedReader(new StringReader(launcher.getStdOut()));
        String lineCol = reader.readLine();
        String moduleLine = reader.readLine();
        if (lineCol != null && moduleLine != null) {
            LineCol declCoord = LineCol.parse(lineCol);
            String moduleName = moduleLine.replaceAll("\"", "");
            return new DeclarationPosition(declCoord, moduleName);
        } else {
            return null;
        }
    }

    public static Module getModule(PsiFile psiFile) {
        return getModule(psiFile.getProject(), psiFile);
    }

    public static Module getModule(Project project, PsiFileSystemItem psiFile) {
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        return getModule(project, file);
    }

    public static Module getModule(Project project, VirtualFile file) {
        return ProjectRootManager.getInstance(project).getFileIndex().getModuleForFile(file);
    }
}
