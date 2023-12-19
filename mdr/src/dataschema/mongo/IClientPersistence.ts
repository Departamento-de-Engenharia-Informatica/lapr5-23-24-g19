export interface IClientPersistence {
    domainId: string
    name: string
    email: string
    phoneNumber: string
    vatNumber: number

    status: string

    password: string
    // salt: string
}
