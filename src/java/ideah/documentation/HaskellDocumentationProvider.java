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
import ideah.util.DeclarationPosition;
import ideah.util.ProcessLauncher;

import java.io.BufferedReader;
import java.io.StringReader;
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
            String sourcePath = CompilerLocation.rootsAsString(module, false);
            ProcessLauncher idLauncher= new ProcessLauncher(
                false, null,
                compiler.exe,
                "-m", "GetIdType",
                "-g", compiler.libPath,
                "-s", sourcePath,
                "--line-number", String.valueOf(line + 1), "--column-number", String.valueOf(col + 1),
                file.getPath()
            );
            String stdOut = idLauncher.getStdOut();
            if (stdOut.trim().isEmpty())
                return null;
            String newMsgIndicator = ProcessLauncher.NEW_MSG_INDICATOR;
            int p = stdOut.indexOf(newMsgIndicator);
            String modName;
            String type;
            if (p >= 0) {
                modName = stdOut.substring(0, p).trim();
                type = stdOut.substring(p + 1).trim();
            } else {
                modName = "?";
                type = "?";
            }
            StringBuilder documentation = new StringBuilder("Module: <code>" + modName + "</code><br>Type: <code>" + type + "</code><br>");
            DeclarationPosition declaration = new DeclarationPosition(line, col, psiFile);
            ProcessLauncher documentationLauncher = new ProcessLauncher(
                false, null,
                compiler.exe,
                "-m", "GetDocu",
                "-g", compiler.libPath,
                "-s", sourcePath,
                "--line-number",
                String.valueOf(declaration.startLine),
                "--column-number", String.valueOf(declaration.startCol),
                "--module", declaration.module
            );
            BufferedReader reader = new BufferedReader(new StringReader(documentationLauncher.getStdOut()));
            String l = reader.readLine();
            while (l != null && !l.startsWith(newMsgIndicator)) {
                l = reader.readLine();
            }
            if (l != null && l.startsWith(newMsgIndicator)) {
                l = reader.readLine();
                while (l != null) {
                    documentation.append("<br>").append(l);
                    l = reader.readLine();
                }
            }
            return documentation.toString();
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
