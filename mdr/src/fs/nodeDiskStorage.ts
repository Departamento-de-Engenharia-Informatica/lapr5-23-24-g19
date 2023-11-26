import { Inject, Service } from 'typedi'
import config from '../../config'
import { IStorageFs } from '../services/IFs/IStorageFs'

import * as fs from 'fs/promises'
import path from 'path'
import { Logger } from 'winston'

@Service()
export default class NodeDiskStorage implements IStorageFs {
    private root: string
    // private logger: Logger

    constructor(@Inject('logger') private logger: Logger) {
        this.root = config.storage.prefix
    }

    async upload(
        filename: string,
        content: object,
        overwrite: boolean = true,
    ): Promise<string> {
        const target = path.join(this.root, filename)

        if ((await this.exists(target)) && !overwrite) {
            this.logger.info(
                'NodeDiskStorage: attempted to overwrite file %s',
                path.dirname(target),
            )
            const err = {
                message: 'File already exists and no overwrite was specified',
                path: target,
            }

            throw new Error(JSON.stringify(err))
        }

        const basedir = path.dirname(target)

        if (!(await this.exists(basedir))) {
            await fs.mkdir(basedir, { recursive: true })
            this.logger.info('NodeDiskStorage: created directory %s', basedir)
        }

        await fs.writeFile(target, JSON.stringify(content))
        this.logger.info('NodeDiskStorage: created file %s', target)

        return filename
    }

    async get<T>(filename: string): Promise<T> {
        const target = path.join(this.root, filename)
        try {
            const content = await fs.readFile(target)
            this.logger.info('NodeDiskStorage: retrieved file %s', target)
            return JSON.parse(content.toString()) as T
        } catch (e) {
            const err = {
                message: 'File does not exist',
                path: target,
            }
            throw new Error(JSON.stringify(err))
        }
    }

    private async exists(filename: string): Promise<boolean> {
        try {
            await fs.access(filename)
            return true
        } catch (_) {
            return false
        }
    }
}
