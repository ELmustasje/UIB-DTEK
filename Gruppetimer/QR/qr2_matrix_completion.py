def set_fixed_fields(matrix, qr_layout):
    for e in qr_layout["fixed_positions"]["zeros"]:
        row = e[0]
        col = e[1]
        matrix[row][col] = 0
    for e in qr_layout["fixed_positions"]["ones"]:
        row = e[0]
        col = e[1]
        matrix[row][col] = 1

def set_meta_fields(matrix, err_corr, mask_no, qr_layout):
    tl = [
        "first",
        "second"
    ]
    mp = qr_layout["meta_patterns"][err_corr][mask_no]
    for pattern_nr in tl:
        pattern = qr_layout["meta_positions"][pattern_nr]
        for i in range(len(pattern)):
            row = pattern[i][0]
            col = pattern[i][1]
            matrix[row][col] = mp[i]

if __name__ == '__main__':
    from qr_dummies import sample_masked_matrix
    from qr1_draw import display
    from pathlib import Path
    import json

    # Retrieve sample of premasked matrix without meta/fixed fields
    # and getting the matching config for it.
    matrix, error_correction_level, mask_no = sample_masked_matrix()
    qr_layout = json.loads(Path('lab8/qr/qrv2_layout.json').read_text(encoding='utf-8'))

    # Try the functions we have created here
    set_meta_fields(matrix, error_correction_level, mask_no, qr_layout)
    set_fixed_fields(matrix, qr_layout)

    # Draw the picture
    display(matrix)
