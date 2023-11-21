export interface IStorageFs {
    upload(filename: string, content: object, overwrite?: boolean): Promise<string>
    get<T>(filename: string): Promise<T>
}
