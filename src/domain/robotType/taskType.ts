export enum TaskType {
    SURVEILLANCE = 'SURVEILLANCE',
    DELIVERY = 'DELIVERY',
}

export namespace TaskType {
    export function toString(s: TaskType): string {
        switch (s) {
            case TaskType.SURVEILLANCE:
                return 'Surveillance'
            case TaskType.DELIVERY:
                return 'Delivery'
            default:
                throw 'Unknown'
        }
    }
    export function toType(s: string): TaskType {
        const type = TaskType[s.trim().toUpperCase()]
        if (!type) {
            throw new Error(`Unknown TaskType: ${type}`)
        }
        return type
    }
}
