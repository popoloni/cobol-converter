from typing import List, Generator
from cobol_converter.config import cfg


def list_cobol_files(exclusion_file_names: List[str] = []) -> Generator:
    """
    Generator function to yield COBOL files in a given directory, excluding specified files.

    :param exclusion_file_names: List of file names to exclude from the result.
    :type exclusion_file_names: List[str]

    :return: A generator yielding Path objects for COBOL files.
    :rtype: Generator[Path, None, None]
    """
    for file in cfg.source_code_dir.rglob("*.COB"):
        if file.stem not in exclusion_file_names:
            yield file


def get_cobol():
    """
    Retrieve cobol code to be converted.
    """
    files = list(list_cobol_files())
    return "\n\n".join([f.read_text(encoding='ISO-8859-1') for f in files])


if __name__ == "__main__":
    from cobol_converter.log_factory import logger

    counter = 0
    for _ in list_cobol_files():
        counter += 1
    assert counter > 0, "No files found"
    cobol = get_cobol()
    assert cobol is not None
    logger.info("\n" + cobol)
