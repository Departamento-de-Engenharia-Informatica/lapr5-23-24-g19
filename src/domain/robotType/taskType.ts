export enum TaskType {
    SURVEILLANCE = 'SURVEILLANCE',
    DELIVERY = 'DELIVERY',
}

export namespace TaskType {
    export function toString(s: TaskType): string {
        switch (s.trim().toUpperCase()) {
            case TaskType.SURVEILLANCE:
                return 'SURVEILLANCE'
            case TaskType.DELIVERY:
                return 'DELIVERY'
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
