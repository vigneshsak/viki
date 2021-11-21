-- Dataset owid-coid-data.csv has been split into CovidDeath & CovidVaccination 

---Performing where & orderby & and
-- Viewing the CovidDeath dataset

SELECT *
from Portfolio..[CovidDeath] as CD
where continent is not null  and [population] is not null
order by [population]desc,[location] 


---Performing like & as alias

-- Viewing Total Cases vs Deaths AS DeathPercentagefor per entry

select [total_cases],[total_deaths],[location],([total_deaths]/[total_cases])*100 as DeathPercentage
from Portfolio..[CovidDeath] as CD
where [location] like '%IND%' and ([total_deaths]/[total_cases])*100  is not null
order by [population] desc, DeathPercentage DESC

-- Viewing Total Cases vs Population  

Select [location],[date] ,[population] ,[total_cases] , (total_cases/population)*100 as PercentPopulationInfected,[total_cases_per_million],[total_deaths_per_million]
From Portfolio..[CovidDeath]
order by [total_cases_per_million] desc, [total_deaths_per_million] desc


---Performing MAX & group by

--Viewing Countries with High Infection Rate to Population

select [location],[population], MAX(total_cases) as HighestInfectionCount,  Max((total_cases/population))*100 as PercentPopulationInfected
From Portfolio..[CovidDeath]
where [continent] is not null --to avoid continents displaying in the location, since the dataset is in such a way 
group by [location],[population]
order by HighestInfectionCount desc, PercentPopulationInfected desc


---Peforming CAST changing the data_type from float to int

--Viewing Countries with Highest Death Count 

select [location],[population], MAX(CAST([total_deaths] as int)) AS TotalDeathCount
From Portfolio..[CovidDeath]
Where [continent] is not null 
--Where [continent] is not null -- TO VIEW THE CONTINENTS ALONE
Group by [location],[population]
order by TotalDeathCount desc


---Peforming Sum && Cast

 --Global view numbers

select  SUM([new_cases]) as total_cases, Sum(cast([new_deaths] as int)) as total_deaths, sum(cast([new_deaths] AS INT))/SUM([new_cases])*100 as DeathPercentage
from Portfolio..[CovidDeath] as CD
Where [continent] is not null 
order by 1

---Performing Join operation
---Performing Patition by

 --Viewing Total Population vs Vaccinated

select  cd.[continent], cd.[location], cd.[date],cd.[population],  v.[new_vaccinations],
SUM(cast(v.new_vaccinations as bigint)) OVER (Partition by cd.Location Order by cd.location, cd.Date) as RollingPeopleVaccinated
from Portfolio..[CovidDeath] as CD
join Portfolio..[CovidVaccination] V
    On cd.location = v.location
    and cd.date = v.date
where cd.continent is not null and v.new_vaccinations is not null
order by 2,3

-- Using CTE to perform Calculation on Partition By in previous query

With PopulationvsVaccination (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as
(

select  cd.[continent], cd.[location], cd.[date],cd.[population],  v.[new_vaccinations],
SUM(cast(v.new_vaccinations as bigint)) OVER (Partition by cd.Location Order by cd.location, cd.Date) as RollingPeopleVaccinated
from Portfolio..[CovidDeath] as CD
join Portfolio..[CovidVaccination] V
    On cd.location = v.location
    and cd.date = v.date
where cd.continent is not null and v.new_vaccinations is not null
--order by 2,3
)
Select *, (RollingPeopleVaccinated/Population)*100 as PeoplVacPercent
From PopulationvsVaccination


---Performing Temp table

-- Using Temp Table to perform Calculation on Partition By in previous query

DROP Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_vaccinations numeric,
RollingPeopleVaccinated numeric
)

--Performing Insert table

Insert into #PercentPopulationVaccinated
select  cd.[continent], cd.[location], cd.[date],cd.[population],  v.[new_vaccinations],
SUM(cast(v.new_vaccinations as bigint)) OVER (Partition by cd.Location Order by cd.location, cd.Date) as RollingPeopleVaccinated
from Portfolio..[CovidDeath] as CD
join Portfolio..[CovidVaccination] V
    On cd.location = v.location
    and cd.date = v.date
where cd.continent is not null and v.new_vaccinations is not null
--order by 2,3
Select *, (RollingPeopleVaccinated/Population)*100
From #PercentPopulationVaccinated

--- Creating View to store data 

Create View PercentPopulationVaccinated as
select  cd.[continent], cd.[location], cd.[date],cd.[population],  v.[new_vaccinations],
SUM(cast(v.new_vaccinations as bigint)) OVER (Partition by cd.Location Order by cd.location, cd.Date) as RollingPeopleVaccinated
from Portfolio..[CovidDeath] as CD
join Portfolio..[CovidVaccination] V
    On cd.location = v.location
    and cd.date = v.date
where cd.continent is not null and v.new_vaccinations is not null
--order by 2,3
