import { Component } from '@angular/core'
import { InhibitRobotDTO } from 'src/app/dto/InhibitRobotDTO'
import { RobotDTO } from 'src/app/dto/RobotDTO'
import { RobotService } from 'src/app/services/robot.service'

@Component({
    selector: 'app-inhibit',
    templateUrl: './inhibit-robot.component.html',
    styleUrls: ['./inhibit-robot.component.css'],
})
export class InhibitRobotComponent {
    private allRobots: RobotDTO[] = []
    robots?: RobotDTO[]

    constructor(private service: RobotService) {}

    ngOnInit() {
        this.listRobots()
    }

    listRobots() {
        this.service.getRobots().subscribe({
            next: (robotList) => {
                this.allRobots = robotList
                this.robots = robotList
            },
            error: (error) => alert(error),
        })
    }

    inhibit(robot: RobotDTO) {
        const dto: InhibitRobotDTO = {
            code: robot.code,
            state: robot.state == 0 ? 1 : 0,
        }

        this.service.inhibit(dto).subscribe({
            next: () => this.listRobots(),
            error: (error) => alert(JSON.stringify(error)),
        })
    }

    // NOTE: refactor out
    stateToString(state: number) {
        switch (state) {
            case 0:
                return 'Enabled'
            case 1:
                return 'Disabled'
            default:
                return 'Unknown'
        }
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.robots = this.allRobots
        } else {
            this.robots = this.allRobots.filter(
                (r) =>
                    r.code.toLowerCase().includes(prop) ||
                    (prop.length > 2 &&
                        (r.nickname?.toLowerCase().includes(prop) ||
                            this.stateToString(r.state).toLowerCase().includes(prop))),
            )
        }
    }
}
