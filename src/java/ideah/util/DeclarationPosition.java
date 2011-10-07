package ideah.util;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

public final class DeclarationPosition {

    public final int startLine;
    public final int startCol;
    public final String module;

    public DeclarationPosition(int line, int col, PsiFile psiFile) throws InterruptedException, IOException {
        VirtualFile file = psiFile.getVirtualFile();
        Module module = ProjectRootManager.getInstance(psiFile.getProject()).getFileIndex().getModuleForFile(file);
        CompilerLocation compiler = CompilerLocation.get(module);
        String sourcePath = CompilerLocation.rootsAsString(module, false);
        ProcessLauncher launcher = new ProcessLauncher(
            false, null,
            compiler.exe,
            "-m", "GetDeclPos",
            "-g", compiler.libPath,
            "-s", sourcePath,
            "--line-number", String.valueOf(line + 1), "--column-number", String.valueOf(col + 1),
            file.getPath()
        );
        BufferedReader reader = new BufferedReader(new StringReader(launcher.getStdOut()));
        String l = reader.readLine();
        String c = reader.readLine();
        if (l != null && c != null) {
            startLine = Integer.parseInt(l);
            startCol = Integer.parseInt(c);
        } else {
            startLine = -1;
            startCol = -1;
        }
        this.module = reader.readLine();
    }
}
