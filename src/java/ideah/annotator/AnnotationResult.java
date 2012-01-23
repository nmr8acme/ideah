package ideah.annotator;

import com.intellij.openapi.vfs.VirtualFile;
import ideah.compiler.GHCMessage;

import java.util.List;

final class AnnotationResult {

    final VirtualFile file;
    final List<GHCMessage> ghcMessages;

    AnnotationResult(VirtualFile file, List<GHCMessage> ghcMessages) {
        this.file = file;
        this.ghcMessages = ghcMessages;
    }
}
