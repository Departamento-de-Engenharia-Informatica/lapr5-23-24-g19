import { error } from "console"

export enum TaskType {
    SURVEILLANCE="SURVEILLANCE",
    DELIVERY="DELIVERY",
}

export namespace TaskType {
    export function toString(s: TaskType): string {
        switch (s) {
            case TaskType.SURVEILLANCE:
                return 'Surveillance'
            case TaskType.DELIVERY:
                return 'Delivery'
            default:
                throw error
        }
    }
    export function toType(s: string): TaskType {
        return TaskType[s.trim().toUpperCase()]
    }
}
