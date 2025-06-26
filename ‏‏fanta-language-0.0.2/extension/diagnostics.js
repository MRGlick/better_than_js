const vscode = require('vscode');

/**
 * Function to scan open files and report errors
 */
function activate(context) {
    let diagnosticCollection = vscode.languages.createDiagnosticCollection('fanta');

    vscode.workspace.onDidChangeTextDocument(event => {
        if (event.document.languageId !== 'fanta') return;
        updateDiagnostics(event.document, diagnosticCollection);
    });

    vscode.workspace.onDidOpenTextDocument(document => {
        if (document.languageId !== 'fanta') return;
        updateDiagnostics(document, diagnosticCollection);
    });

    context.subscriptions.push(diagnosticCollection);
}

/**
 * Function to check for compilation errors
 */
function updateDiagnostics(document, diagnosticCollection) {
    const diagnostics = [];
    const text = document.getText();
    const lines = text.split(/\r?\n/);

    for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
        const line = lines[lineIndex];

        // Example 1: Check for missing semicolons
        if (/^\s*(int|float|string|bool)\s+[a-zA-Z_]\w*\s*=?\s*[^;]*$/.test(line)) {
            const range = new vscode.Range(lineIndex, line.length - 1, lineIndex, line.length);
            diagnostics.push(new vscode.Diagnostic(range, "Missing semicolon ';'", vscode.DiagnosticSeverity.Error));
        }

        // Example 2: Check for undefined variable usage
        if (/[^a-zA-Z0-9_](undefinedVar)[^a-zA-Z0-9_]/.test(line)) {
            const varStart = line.indexOf("undefinedVar");
            const range = new vscode.Range(lineIndex, varStart, lineIndex, varStart + "undefinedVar".length);
            diagnostics.push(new vscode.Diagnostic(range, "Undefined variable: undefinedVar", vscode.DiagnosticSeverity.Warning));
        }

        // Example 3: Check for empty if() and while() parentheses
        const emptyIfWhileMatch = line.match(/\b(if|while)\s*\(\s*\)/);
        if (emptyIfWhileMatch) {
            const keywordStart = line.indexOf(emptyIfWhileMatch[0]);
            const range = new vscode.Range(lineIndex, keywordStart, lineIndex, keywordStart + emptyIfWhileMatch[0].length);
            diagnostics.push(new vscode.Diagnostic(range, `"${emptyIfWhileMatch[1]}" statement cannot have empty parentheses`, vscode.DiagnosticSeverity.Error));
        }
    }

    diagnosticCollection.set(document.uri, diagnostics);
}

module.exports = {
    activate
};
