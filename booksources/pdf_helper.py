import os
import sys
import fitz  # PyMuPDF

BOOKSOURCES_DIR = os.path.dirname(os.path.abspath(__file__))
EXTRACTED_DIR = os.path.join(BOOKSOURCES_DIR, "extracted_text")

# Ensure UTF-8 output even on Windows command prompt
if sys.platform == "win32":
    try:
        sys.stdout.reconfigure(encoding='utf-8')
    except Exception:
        pass

def extract_all():
    os.makedirs(EXTRACTED_DIR, exist_ok=True)
    pdf_files = [f for f in os.listdir(BOOKSOURCES_DIR) if f.endswith(".pdf")]
    
    print(f"Found {len(pdf_files)} PDF files in {BOOKSOURCES_DIR}")
    for pdf_file in pdf_files:
        pdf_path = os.path.join(BOOKSOURCES_DIR, pdf_file)
        fname = os.path.splitext(pdf_file)[0]
        out_txt_path = os.path.join(EXTRACTED_DIR, f"{fname}.txt")
        
        if os.path.exists(out_txt_path):
            print(f"Already extracted: {fname}")
            continue
            
        print(f"Extracting {pdf_file} with PyMuPDF...")
        doc = fitz.open(pdf_path)
        pages_content = []
        for page_num in range(len(doc)):
            page = doc[page_num]
            text = page.get_text()
            pages_content.append(f"=== {pdf_file} | PAGE {page_num + 1} ===\n{text}\n")
        
        with open(out_txt_path, "w", encoding="utf-8", errors="ignore") as f:
            f.write("\n".join(pages_content))
        print(f"Saved {out_txt_path} ({len(doc)} pages)")

def search_text(query, max_results=10):
    if not os.path.exists(EXTRACTED_DIR):
        print("Extracting text first...")
        extract_all()
        
    query_lower = query.lower()
    txt_files = [f for f in os.listdir(EXTRACTED_DIR) if f.endswith(".txt")]
    results = []
    
    for txt_file in txt_files:
        txt_path = os.path.join(EXTRACTED_DIR, txt_file)
        with open(txt_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()
            
        pages = content.split("=== ")
        for page in pages:
            if not page.strip():
                continue
            lines = page.splitlines()
            header = lines[0] if lines else ""
            body = "\n".join(lines[1:])
            
            if query_lower in body.lower():
                idx = body.lower().find(query_lower)
                start = max(0, idx - 150)
                end = min(len(body), idx + 250)
                snippet = body[start:end].replace("\n", " ")
                # Clean snippet for stdout safety
                safe_snippet = snippet.encode("utf-8", errors="replace").decode("utf-8")
                results.append((header, safe_snippet))
                if len(results) >= max_results:
                    break
        if len(results) >= max_results:
            break
            
    print(f"\n--- Search Results for '{query}' ({len(results)} found) ---")
    for header, snippet in results:
        print(f"\nLocation: === {header}")
        print(f"Snippet: ...{snippet}...")

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "search":
        q = " ".join(sys.argv[2:]) if len(sys.argv) > 2 else "epidemiology"
        search_text(q)
    else:
        extract_all()
