package ideah.documentation;

import com.intellij.lang.documentation.DocumentationProvider;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import ideah.psi.api.HPIdent;
import ideah.util.CompilerLocation;
import ideah.util.ProcessLauncher;

import java.util.List;

public final class HaskellDocumentationProvider implements DocumentationProvider {

    private static final Logger LOG = Logger.getInstance("ideah.documentation.HaskellDocumentationProvider");

    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        return null;
    }

    public List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        return null; // todo: find external docs
    }

    public String generateDoc(PsiElement element, PsiElement originalElement) {
        if (!(element instanceof HPIdent))
            return null;
        HPIdent ident = (HPIdent) element;
        TextRange range = ident.getTextRange();
        PsiFile psiFile = element.getContainingFile();
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        FileDocumentManager fdm = FileDocumentManager.getInstance();
        Document doc = fdm.getCachedDocument(file);
        if (doc == null)
            return null;
        // todo: run in event-dispatch thread
        //fdm.saveAllDocuments();
        int offset = range.getStartOffset();
        int line = doc.getLineNumber(offset);
        int col = offset - doc.getLineStartOffset(line);
        Module module = ProjectRootManager.getInstance(psiFile.getProject()).getFileIndex().getModuleForFile(file);
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null) {
            return null;
        }
        try {
            ProcessLauncher idLauncher= new ProcessLauncher(
                false, null,
                compiler.exe,
                "-m", "GetIdType",
                "-g", compiler.libPath,
                "-s", CompilerLocation.rootsAsString(module, false),
                "--line-number", String.valueOf(line + 1), "--column-number", String.valueOf(col + 1),
                file.getPath()
            );
            String stdOut = idLauncher.getStdOut();
            if (stdOut.trim().isEmpty())
                return null;
            int p = stdOut.indexOf('\f');
            String modName;
            String type;
            if (p >= 0) {
                modName = stdOut.substring(0, p).trim();
                type = stdOut.substring(p + 1).trim();
            } else {
                modName = "?";
                type = "?";
            }
            // todo: temporarily removed
//                ProcessLauncher docuLauncher = new ProcessLauncher(
//                    false, null,
//                    compiler.exe,
//                    "-m", "GetDocu",
//                    "--line-number", String.valueOf(line + 1), "--column-number", String.valueOf(col + 1),
//                    file.getPath()
//                );
//                String docs = docuLauncher.getStdOut();
            return "Module: <code>" + modName + "</code><br>Type: <code>" + type + "</code><br>";
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }

    public PsiElement getDocumentationElementForLookupItem(PsiManager psiManager, Object object, PsiElement element) {
        return null;
    }

    public PsiElement getDocumentationElementForLink(PsiManager psiManager, String link, PsiElement context) {
        return null;
    }
}
