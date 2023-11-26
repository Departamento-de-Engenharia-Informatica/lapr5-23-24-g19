export enum RobotState {
    ENABLED,
    DISABLED,
}

export namespace RobotState {
    export function toString(s: RobotState): string {
        switch (s) {
            case RobotState.ENABLED:
                return 'Enabled'
            case RobotState.DISABLED:
                return 'Disabled'
            default:
                return 'Unknown'
        }
    }

    export function fromCode(code: number): RobotState {
        switch (code) {
            case 0:
                return RobotState.ENABLED
            case 1:
                return RobotState.DISABLED
            default:
                throw new Error(`Invalid code: ${code}`)
        }
    }
}
