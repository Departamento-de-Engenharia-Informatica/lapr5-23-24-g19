import { Component, OnInit } from '@angular/core'
import { RobotDTO, RobotService } from 'src/app/services/robot.service'

@Component({
    selector: 'app-list-robots',
    templateUrl: './list-robots.component.html',
    styleUrls: ['./list-robots.component.css'],
})
export class ListRobotsComponent implements OnInit {
    robots: RobotDTO[]
    allRobots: RobotDTO[]

    constructor(private robotService: RobotService) {
        this.robots = []
        this.allRobots = []
    }

    ngOnInit(): void {
        this.robotService.getRobots().subscribe((list: RobotDTO[]) => {
            this.allRobots = list
            this.robots = this.allRobots
        })
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.robots = this.allRobots
        } else {
            this.robots = this.allRobots.filter(
                (b) =>
                    b.code.toLowerCase().includes(prop) ||
                    (prop.length > 2 && b.description?.toLowerCase().includes(prop)),
            )
        }
    }
}
