export type FilePath = string

interface _CreateOptions {
    workdirBase: string
}

export type CreateOptions = Partial<_CreateOptions>

export default interface IArchiveService {
    createArchive(
        content: { [file: string]: any },
        options?: CreateOptions,
    ): Promise<FilePath>
}
