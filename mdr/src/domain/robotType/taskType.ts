export enum TaskType {
    SURVEILLANCE = 'SURVEILLANCE',
    DELIVERY = 'DELIVERY',
}

export namespace TaskType {
    export function toString(s: TaskType): string {
        switch (s) {
            case TaskType.SURVEILLANCE:
                return 'SURVEILLANCE'
            case TaskType.DELIVERY:
                return 'DELIVERY'
        }
    }
    export function toType(s: string): TaskType {
        const type = TaskType[s]
        if (!type) {
            throw new Error(`Unknown TaskType: ${type}`)
        }
        return type
    }
}
