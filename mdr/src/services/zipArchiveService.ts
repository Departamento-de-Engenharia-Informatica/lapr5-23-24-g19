import { Service } from 'typedi'
import IArchiveService, { CreateOptions, FilePath } from './IServices/IArchiveService'

import * as archiver from 'archiver'
import * as crypto from 'crypto'

import * as fs from 'fs/promises'
import * as path from 'path'

@Service()
export default class ZipService implements IArchiveService {
    async createArchive(
        content: { [file: string]: any },
        options: CreateOptions = {},
    ): Promise<FilePath> {
        let outputHandle: fs.FileHandle

        try {
            const dirHash = crypto
                .createHash('sha512')
                .update(options.workdirBase ?? '__client__' + new Date().toISOString())
                .digest('hex')

            const tempDir = path.join('.', 'temp-gdpr', dirHash)
            await fs.mkdir(tempDir, { recursive: true })

            const zipPath = path.join(tempDir, 'data.zip')
            outputHandle = await fs.open(zipPath, 'w')
            const output = outputHandle.createWriteStream()

            const archive = archiver.create('zip', { zlib: { level: 9 } })

            archive.pipe(output)

            Object.entries(content).forEach(([service, data]) => {
                const fileName = `${service}.json`
                const fileContent = JSON.stringify(data, null, 2)
                archive.append(fileContent, { name: fileName })
            })

            await archive.finalize()

            return zipPath
        } catch (error) {
            throw new Error('Error creating ZIP file: ' + error.message)
        } finally {
            if (outputHandle) {
                await outputHandle.close()
            }
        }
    }
}
