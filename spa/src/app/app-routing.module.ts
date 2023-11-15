import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BuildingComponent } from './components/building/building.component';
import { CreateElevatorComponent } from './components/elevator/create-elevator/create-elevator.component';
import { ListElevatorsComponent } from './components/elevator/list-elevators/list-elevators.component';
import { EditBuildingComponent } from './components/edit-building/edit-building.component';
import { ElevatorComponent } from './components/elevator/elevator.component';
import { FloorComponent } from './components/floor/floor.component';
import { ListFloorsComponent } from './components/floor/list-floors/list-floors.component';
import { GetBuildingsComponent } from './components/get-buildings/get-buildings.component';
import { MainMenuComponent } from './components/main-menu/main-menu.component';
import { ModulesComponent } from './components/modules/modules.component';
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component';
import { PassageComponent } from './components/passage/passage.component';
import { RobotTypeComponent } from './components/robot-type/robot-type.component';
import { RobotComponent } from './components/robot/robot.component';
import { RoomComponent } from './components/room/room.component';
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component';
import { ListPassagesBetweenBuildingsComponent } from './components/passage/list-passages-between-buildings/list-passages-between-buildings.component';
import { ListBuildingsMinmaxFloorsComponent } from './components/building/list-buildings-minmax-floors/list-buildings-minmax-floors.component';
import { CreateFloorComponent } from './components/floor/create-floor/create-floor.component';

export const routes: Routes = [
    { path: '', component: ModulesComponent, title: 'Home page' },
    { path: 'modules', component: ModulesComponent, title: 'Modules page' },
    { path: 'menu', component: MainMenuComponent, title: 'Home page' },
    { path: 'buildings', component: BuildingComponent, title: 'Buildings' },
    {
        path: 'buildings/edit',
        component: EditBuildingComponent,
        title: 'Edit Building',
    },
    {
        path: 'buildings/list',
        component: GetBuildingsComponent,
        title: 'List Buildings',
    },
    {
        path: 'buildings/list-by-floors',
        component: ListBuildingsMinmaxFloorsComponent,
        title: 'List Buildings by minimum and maximum number of Floors',
    },
    // children: [
    //   { path: 'list', component: GetBuildingsComponent,title: 'List of Buildings'},
    // ]},
    //TODO: Properly route each component
    { path: 'floors', component: FloorComponent, title: 'Floors' },
    {
        path: 'floors/list',
        component: ListFloorsComponent,
        title: 'List Floors',
    },
    {
        path: 'floors/create',
        component: CreateFloorComponent,
        title: 'List Floors',
    },
    { path: 'elevators', component: ElevatorComponent },
    {
        path: 'elevators/create',
        component: CreateElevatorComponent,
        title: 'Create Elevator',
    },
    {
        path: 'elevators/list',
        component: ListElevatorsComponent,
        title: 'List Elevators',
    },
    { path: 'rooms', component: RoomComponent },
    { path: 'passages', component: PassageComponent },
    {
        path: 'passages/list-passages-between-buildings',
        component: ListPassagesBetweenBuildingsComponent,
        title: 'List passages between buildings',
    },
    { path: 'robot-types', component: RobotTypeComponent },
    { path: 'robots', component: RobotComponent },
    { path: '3D-visualization', component: Visualization3DComponent },
    { path: 'campus', component: MainMenuComponent, title: 'Home page' },
    { path: '**', component: PageNotFoundComponent },
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule],
})
export class AppRoutingModule {}
