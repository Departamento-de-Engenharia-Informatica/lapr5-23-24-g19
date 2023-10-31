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
}
